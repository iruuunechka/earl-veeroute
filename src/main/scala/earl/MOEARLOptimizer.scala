package earl

import java.io.PrintWriter
import java.util.concurrent.ThreadLocalRandom

import earl.webio.VeeRouteService

import scala.annotation.tailrec

object MOEARLOptimizer {
  private val tolerance = 1e-4

  @tailrec
  private def compareImpl(l: Seq[Double], r: Seq[Double], i: Int): Int = {
    if (i == l.size) {
      0
    } else if (l(i) + tolerance < r(i)) {
      1
    } else if (l(i) > tolerance + r(i)) {
      -1
    } else compareImpl(l, r, i + 1)
  }

  @tailrec
  private def dominatesImpl(l: Seq[Double], r: Seq[Double], i: Int): Boolean = {
    if (i == l.size) {
      true
    } else if (l(i) > tolerance + r(i)) {
      false
    } else dominatesImpl(l, r, i + 1)
  }

  def runOnDataset(summary: PrintWriter, budget: Int, run: Int)
    (service: Service)(d: service.Dataset): Unit = {

    val refName = d.reference.name
    assert(refName.endsWith(".json.bz2"))
    val outputFileName = refName.substring(0, refName.length - ".json.bz2".length) + "_saved_run.json"

    val heading = s"Dataset #${d.reference.number} run #$run: ${d.reference.name}"
    summary.println(heading)
    summary.flush()

    val functions = d.functions
    val publicFunctions = functions.filter(_.isPublic)
    val random = ThreadLocalRandom.current()

    object IndividualCollection {
      import scala.collection.mutable.ArrayBuffer

      private implicit val individualOrdering: Ordering[AbstractIndividual] = (l, r) => compareImpl(l.fitness, r.fitness, 0)

      private val globalIndexTree = new IndexTree[AbstractIndividual]() // TODO: from the database. Determines the ranks
      private val localIndexTree = new IndexTree[Individual]()  // For local unification (no need of TreeMap)
      private val ranks = new ArrayBuffer[ArrayBuffer[Individual]]()

      abstract class AbstractIndividual {
        private[IndividualCollection] def fitness: Seq[Double]
        private[IndividualCollection] def publicObjectives: Seq[Double]

        def nObjectives: Int = publicObjectives.size + 1
        def objective(index: Int): Double = if (index == 0) globalIndexTree.indexOf(this) else publicObjectives(index - 1)
      }

      class Individual private[IndividualCollection] (val handle: d.Individual) extends AbstractIndividual {
        // this is for comparison and rank determination
        private[IndividualCollection] override val fitness = functions.map(handle.fitness)
        // this is for NSGA-II
        private[IndividualCollection] override val publicObjectives = publicFunctions.map(handle.fitness)

        private[IndividualCollection] var rank: Int = 0
        private[IndividualCollection] var crowdingDistance: Double = 0

        def isDominatedBy(that: Individual): Boolean = dominatesImpl(that.publicObjectives, publicObjectives, 0)
      }

      def rankStats: String = ranks.zipWithIndex.map(p => (p._2, p._1.size)).mkString(", ")

      def last: Individual = localIndexTree.last

      def consume(handle: d.Individual): Individual = {
        val ind = new Individual(handle)
        globalIndexTree.add(ind)
        if (localIndexTree.add(ind)) {
          // Reevaluate the NSGA-II ranks
          ranks.foreach(_.clear())

          if (ranks.size == 0) {
            ranks += new ArrayBuffer[IndividualCollection.Individual]()
          }
          var index = localIndexTree.size - 1
          while (index >= 0) {
            val curr = localIndexTree.valueAt(index)
            val better = (index + 1 until localIndexTree.size).view.map(localIndexTree.valueAt).filter(curr.isDominatedBy).map(_.rank)
            curr.rank = if (better.isEmpty) 0 else better.max + 1
            curr.crowdingDistance = 0
            if (ranks.size == curr.rank) {
              ranks += new ArrayBuffer[IndividualCollection.Individual]()
            }
            ranks(curr.rank) += curr
            index -= 1
          }

          // Reevaluate the NSGA-II crowding distances
          for (rankSet <- ranks if rankSet.nonEmpty) {
            for (obj <- 0 until localIndexTree.head.nObjectives) {
              val sorted = rankSet.sortBy(_.objective(obj))
              sorted.head.crowdingDistance = Double.PositiveInfinity
              sorted.last.crowdingDistance = Double.PositiveInfinity
              val delta = sorted.last.objective(obj) - sorted.head.objective(obj)
              for (i <- 1 until sorted.size - 1) {
                sorted(i).crowdingDistance += (sorted(i + 1).objective(obj) - sorted(i - 1).objective(obj)) / delta
              }
            }
          }
        }
        globalIndexTree.validate()
        localIndexTree.validate()

        localIndexTree.sameAs(ind)
      }

      def choose(): Individual = {
        val a, b = localIndexTree.valueAt(random.nextInt(localIndexTree.size))
        if (a.rank != b.rank) {
          if (a.rank < b.rank) a else b
        } else if (a.crowdingDistance != b.crowdingDistance) {
          if (a.crowdingDistance > b.crowdingDistance) a else b
        } else {
          if (random.nextBoolean()) a else b
        }
      }
    }

    def fitnessToString(fitness: Map[d.Function, Double]): String = {
      fitness.map(p => (p._1.number, p._2)).toIndexedSeq.sortBy(_._1).mkString("[", ", ", "]")
    }

    var last = IndividualCollection.consume(d.individuals.head)
    summary.println(s"1: ${fitnessToString(last.handle.fitness)}")
    summary.flush()

    case class OptimizationAct(
      source: d.Individual,
      target: d.Individual,
      optimizer: Int,
      firstObjective: Int,
      time: Long
    )

    val acts = IndexedSeq.newBuilder[OptimizationAct]

    for (t <- 2 to budget) {
      val curr = IndividualCollection.choose().handle
      val optimizerIndex = random.nextInt(service.optimizers.size)
      val optimizer = service.optimizers(optimizerIndex)
      val firstFunctionIndex = random.nextInt(functions.size)
      val firstFunction = functions(firstFunctionIndex)
      val action = firstFunction +: functions.filter(_ != firstFunction)
      println()
      println(s"Step $t:")
      println(s"  From ${curr.id}")
      println(s"      Fitness ${fitnessToString(curr.fitness)}")
      println(s"  By optimizer ${optimizer.number} (${optimizer.name})")
      println(s"  Using functions ${action.mkString(", ")}")

      val startTime = System.currentTimeMillis()
      val next = curr.optimize(optimizer, action :_*)
      val optimizationTime = System.currentTimeMillis() - startTime

      acts += OptimizationAct(curr, next, optimizerIndex, firstFunctionIndex, optimizationTime)

      println("Result:")
      println(s"  ${next.id}")
      println(s"      Fitness ${fitnessToString(next.fitness)}")
      IndividualCollection.consume(next)
      val newLast = IndividualCollection.last
      if (!(newLast eq last)) {
        last = newLast
        summary.println(s"$t: ${fitnessToString(last.handle.fitness)}")
        summary.flush()
      }
      println(s"Rank stats: ${IndividualCollection.rankStats}")
    }

    val problemName = d.reference.name
    val functionNames = functions.map(_.name)
    val optimizerNames = service.optimizers.map(_.name)
    val individualMap = d.individuals.zipWithIndex.toMap
    val mappedActs = acts.result().map(a => RunDatabase.OptimizationAct(
      source = individualMap(a.source),
      target = individualMap(a.target),
      optimizer = a.optimizer,
      firstObjective = a.firstObjective,
      time = a.time
    ))

    val outputDatabase = RunDatabase(
      problemName = problemName,
      objectives = functionNames,
      optimizers = optimizerNames,
      individuals = d.individuals.map(i => functions.map(i.fitness)),
      acts = mappedActs
    )
    outputDatabase.saveTo(outputFileName)
  }

  def main(args: Array[String]): Unit = {
    val srv = VeeRouteService
    val summary = new PrintWriter(args(0))
    val budget = 20
    try {
      for (idx <- 0 to 8; run <- 0 until 1) {
        srv.withDataset(srv.datasets(idx))(runOnDataset(summary, budget, run)(srv))
      }
    } finally {
      summary.close()
    }
  }
}
