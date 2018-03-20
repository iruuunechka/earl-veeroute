package earl

import earl.webio.VeeRouteService

import scala.collection.mutable.{ArrayBuffer, HashMap => MuHashMap}

object UCBOptimizer {
  private val tolerance = 1e-4

  def runOnDataset(d: VeeRouteService.Dataset): Unit = {
    val functions = d.functions

    case class IndividualWithRank(individual: d.Individual, var rank: Int)

    val remap = new MuHashMap[Map[d.Function, Double], IndividualWithRank]()
    val uniqueSortedIndividuals = new ArrayBuffer[IndividualWithRank]()

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
        hands ++= functions.map(f => new BanditHand(ind, IndexedSeq(f), iwr))
      }
      iwr
    }

    class BanditHand(val source: d.Individual, val action: Seq[d.Function], ascendant: IndividualWithRank) {
      private val children = new ArrayBuffer[IndividualWithRank]()

      def estimation(totalCount: Int): Double = {
        if (children.isEmpty) {
          // never sampled before
          ascendant.rank + math.sqrt(2 * math.log(totalCount / 0.5))
        } else {
          // evaluate according to the rules
          val expectedRank = children.iterator.map(_.rank.toDouble).sum / children.size
          expectedRank + math.sqrt(2 * math.log(totalCount / children.size))
        }
      }

      def sample(): Seq[BanditHand] = {
        val newIndividual = rehash(source.optimize(action :_*))
        println(s"Result: $newIndividual")
        children += newIndividual
        if (children.size == 1) {
          functions.filterNot(action.contains).map(f => new BanditHand(source, action :+ f, newIndividual))
        } else Seq.empty
      }
    }

    rehash(d.individuals.head)
    var totalCount = 1
    while (true) {
      val best = hands.maxBy(_.estimation(totalCount))
      println()
      println(s"Current best fitness: ${uniqueSortedIndividuals.last.individual.fitness}")
      println(s"Step $totalCount: Selecting hand {")
      println(s"  individual: ${best.source}")
      println(s"  action: ${best.action}")
      println(s"  estimation: ${best.estimation(totalCount)}")
      println("}")
      hands ++= best.sample()
      totalCount += 1
    }
  }

  def main(args: Array[String]): Unit = {
    val srv = VeeRouteService
    val idx = args(0).toInt
    srv.withDataset(srv.datasets(idx))(runOnDataset)
  }
}
