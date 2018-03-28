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

  def runOnDataset(summary: PrintWriter, graphs: PrintWriter, budget: Int, run: Int)
    (service: Service)(d: service.Dataset): Unit = {

    val heading = s"Dataset #${d.reference.number} run #$run: ${d.reference.name}"
    summary.println(heading)
    summary.flush()

    val functions = d.functions
    val publicFunctions = functions.filter(_.isPublic)
    val random = ThreadLocalRandom.current()

    object IndividualCollection {
      import scala.collection.mutable.ArrayBuffer

      private val globalIndexTree = new IndexTree[Individual]() // TODO: from the database. Determines the ranks
      private val localIndexTree = new IndexTree[Individual]()  // For local unification (no need of TreeMap)
      private val ranks = new ArrayBuffer[ArrayBuffer[Individual]]()

      class Individual private[IndividualCollection] (val handle: d.Individual) extends Ordered[Individual] {
        // this is for comparison and rank determination
        private[IndividualCollection] val fitness = functions.map(handle.fitness)
        // this is for NSGA-II
        private[IndividualCollection] val publicObjectives = publicFunctions.map(handle.fitness)

        def nObjectives: Int = publicFunctions.size + 1
        def objective(index: Int): Double = if (index == 0) globalIndexTree.indexOf(this) else publicObjectives(index - 1)

        private[IndividualCollection] var rank: Int = 0
        private[IndividualCollection] var crowdingDistance: Double = 0

        def isDominatedBy(that: Individual): Boolean = dominatesImpl(that.publicObjectives, publicObjectives, 0)

        override def compare(that: Individual): Int = compareImpl(fitness, that.fitness, 0)
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

    for (t <- 2 to budget) {
      val curr = IndividualCollection.choose().handle
      val optimizer = service.optimizers(random.nextInt(service.optimizers.size))
      val firstFunction = functions(random.nextInt(functions.size))
      val action = firstFunction +: functions.filter(_ != firstFunction)
      println()
      println(s"Step $t:")
      println(s"  From ${curr.id}")
      println(s"      Fitness ${fitnessToString(curr.fitness)}")
      println(s"  By optimizer ${optimizer.number} (${optimizer.name})")
      println(s"  Using functions ${action.mkString(", ")}")
      val next = curr.optimize(optimizer, action :_*)
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
  }


  final def safeWrapper(summary: PrintWriter)(fun: => Any): Unit = {
    try {
      fun
    } catch {
      case th: Throwable =>
        // Something is broken
        summary.println("#Error: iteration is broken")
        th.printStackTrace(summary)
        th.printStackTrace()
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
      for (idx <- 0 until 1; run <- 0 until 2) {
        safeWrapper(summary) {
          srv.withDataset(srv.datasets(idx))(runOnDataset(summary, graphs, budget, run)(srv))
        }
      }
    } finally {
      summary.close()
      graphs.close()
    }
  }
}
