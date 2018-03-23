package earl

import java.io.PrintWriter

import earl.webio.VeeRouteService

import scala.collection.mutable.{ArrayBuffer, HashMap => MuHashMap}

object UCBOptimizer {
  private val tolerance = 1e-4

  def runOnDataset(summary: PrintWriter, graphs: PrintWriter, budget: Int, run: Int)(d: VeeRouteService.Dataset): Unit = {
    val functions = d.functions
    val optimizers = VeeRouteService.optimizers

    val heading = s"Dataset #${d.reference.number} run #$run: ${d.reference.name}"
    summary.println(heading)
    graphs.println(heading)

    case class IndividualWithRank(individual: d.Individual, var rank: Int)

    val remap = new MuHashMap[Map[d.Function, Double], IndividualWithRank]()
    val uniqueSortedIndividuals = new ArrayBuffer[IndividualWithRank]()

    def fitnessToString(fitness: Map[d.Function, Double]): String = {
      fitness.map(p => (p._1.number, p._2)).toIndexedSeq.sortBy(_._1).mkString("[", ", ", "]")
    }

    def compareFitnessValues(l: Map[d.Function, Double], r: Map[d.Function, Double]): Int = {
      def compareIndex(i: Int): Int = if (i == functions.size) 0 else {
        val left = l(functions(i))
        val right = r(functions(i))
        if (math.abs(left - right) < tolerance) {
          compareIndex(i + 1)
        } else if (left > right) -1 else 1
      }
      compareIndex(0)
    }

    def compareWrappers(left: IndividualWithRank, right: IndividualWithRank): Int = {
      compareFitnessValues(left.individual.fitness, right.individual.fitness)
    }

    val hands = new ArrayBuffer[BanditHand]()

    def rehash(ind: d.Individual): IndividualWithRank = {
      val iwr = remap.getOrElseUpdate(ind.fitness, IndividualWithRank(ind, -1))
      if (iwr.rank == -1) {
        var index = uniqueSortedIndividuals.size
        iwr.rank = index
        uniqueSortedIndividuals += iwr
        while (index > 0 && compareWrappers(uniqueSortedIndividuals(index - 1), uniqueSortedIndividuals(index)) > 0) {
          val tmp = uniqueSortedIndividuals(index - 1)
          uniqueSortedIndividuals(index - 1) = uniqueSortedIndividuals(index)
          uniqueSortedIndividuals(index) = tmp
          uniqueSortedIndividuals(index - 1).rank = index - 1
          uniqueSortedIndividuals(index).rank = index
          index -= 1
        }
        hands ++= optimizers.flatMap(o => functions.map(f => new BanditHand(iwr, o, IndexedSeq(f), iwr)))
      }
      iwr
    }

    class BanditHand(
      val source: IndividualWithRank,
      val optimizer: VeeRouteService.OptimizerReference,
      val action: Seq[d.Function], ascendant: IndividualWithRank
    ) {
      val children = new ArrayBuffer[IndividualWithRank]()

      def estimation(totalCount: Int): Double = {
        if (children.isEmpty) {
          // never sampled before
          ascendant.rank + math.sqrt(4 * math.log(totalCount))
        } else {
          // evaluate according to the rules
          val expectedRank = children.iterator.map(_.rank.toDouble).sum / children.size
          expectedRank + math.sqrt(2 * math.log(totalCount) / children.size)
        }
      }

      def sample(): Seq[BanditHand] = {
        val newIndividual = rehash(source.individual.optimize(optimizer, action :_*))
        println(s"Result ID: ${newIndividual.individual.id}")
        println(s"Result fitness: ${fitnessToString(newIndividual.individual.fitness)}")
        println(s"Result rank: ${newIndividual.rank}")
        children += newIndividual
        if (children.size == 1) {
          functions.filterNot(action.contains).map(f => new BanditHand(source, optimizer, action :+ f, newIndividual))
        } else Seq.empty
      }
    }

    rehash(d.individuals.head)
    summary.println(s"1: ${fitnessToString(d.individuals.head.fitness)}")
    summary.flush()

    var totalCount = 1
    while (totalCount < budget) {
      val prevLast = uniqueSortedIndividuals.last
      val best = hands.maxBy(_.estimation(totalCount))
      println()
      println(s"Current best fitness: ${fitnessToString(uniqueSortedIndividuals.last.individual.fitness)}")
      println(s"Step $totalCount: Selecting hand {")
      println(s"  individual ID: ${best.source.individual.id}")
      println(s"  individual fitness: ${fitnessToString(best.source.individual.fitness)}")
      println(s"  optimizer: ${best.optimizer.number}")
      println(s"  action: ${best.action}")
      println(s"  estimation: ${best.estimation(totalCount)}")
      println("}")
      hands ++= best.sample()
      totalCount += 1
      if (!(prevLast eq uniqueSortedIndividuals.last)) {
        summary.println(s"$totalCount: ${fitnessToString(uniqueSortedIndividuals.last.individual.fitness)}")
        summary.flush()
      }
    }

    graphs.println(s"${uniqueSortedIndividuals.size} individuals")
    for (iwr <- uniqueSortedIndividuals) {
      graphs.println(s"Individual #${iwr.rank}")
      graphs.println(s"    ID: ${iwr.individual.id}")
      graphs.println(s"    Fitness: ${fitnessToString(iwr.individual.fitness)}")
    }
    val nonEmptyHands = hands.filter(_.children.nonEmpty)
    graphs.println(s"${nonEmptyHands.size} hands")
    for (h <- nonEmptyHands) {
      graphs.println(s"Hand source: ${h.source.rank}")
      graphs.println(s"     optimizer: ${h.optimizer.number}")
      graphs.println(s"     action: ${h.action.mkString(" ")}")
      graphs.println(s"     children: ${h.children.map(_.rank).mkString(" ")}")
    }
    graphs.flush()
  }

  final def safeWrapper(summary: PrintWriter)(fun: => Any): Unit = {
    try {
      fun
    } catch {
      case th: Throwable =>
        // Something is broken
        summary.println("#Error: iteration is broken")
        th.printStackTrace(summary)
        summary.println("#Starting over")
        safeWrapper(summary)(fun)
    }
  }

  def main(args: Array[String]): Unit = {
    val srv = VeeRouteService
    val summary = new PrintWriter(args(0))
    val graphs = new PrintWriter(args(1))
    val budget = 100
    try {
      for (idx <- 0 until 2; run <- 0 until 5) {
        safeWrapper(summary) {
          srv.withDataset(srv.datasets(idx))(runOnDataset(summary, graphs, budget, run))
        }
      }
    } finally {
      summary.close()
      graphs.close()
    }
  }
}
