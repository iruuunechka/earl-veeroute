package earl

import java.io.{File, FileInputStream, IOException, PrintWriter}
import java.time.LocalDateTime
import java.util.Properties
import java.util.concurrent.ThreadLocalRandom

import earl.reinforce.QMatrix
import earl.util.Relations._
import earl.webio.VeeRouteService

object MOEARLOptimizer {
  private[this] def dateTimeToString(v: LocalDateTime): String = {
    f"_${v.getYear}%04d${v.getMonthValue}%02d${v.getDayOfMonth}%02d-${v.getHour}%02d${v.getMinute}%02d${v.getSecond}%02d"
  }

  def runOnDataset(databases: Seq[RunDatabase], resultRoot: File, summary: PrintWriter, budget: Int, run: Int)
                  (service: Service)(d: service.Dataset): Unit =
  {
    import scala.collection.mutable.ArrayBuffer

    val date = LocalDateTime.now()
    val refName = d.reference.name
    assert(refName.endsWith(".json.bz2"))
    val outputFile = new File(resultRoot,
                              refName.substring(0, refName.length - ".json.bz2".length) + dateTimeToString(date) + ".json")

    val heading = s"Dataset #${d.reference.number} run #$run: ${d.reference.name}"
    summary.println(heading)
    summary.flush()

    val optimizers = service.optimizers
    val functions = d.functions
    val publicFunctions = functions.filter(_.isPublic)
    val random = ThreadLocalRandom.current()

    case class OptimizationAct(source: d.Individual, target: d.Individual,
                               optimizer: Int, firstObjective: Int, time: Long)

    val acts = new ArrayBuffer[OptimizationAct]

    val localQ = new QMatrix(optimizers.size, functions.size)
    val globalQ = QMatrix.fromDatabases(databases, functions.map(_.name), optimizers.map(_.name))

    object IndividualCollection {

      private implicit val individualOrdering: Ordering[Individual] = (l, r) => compareSeq(l.fitness, r.fitness)

      private val indexTree = new IndexTree[Individual]()  // For local unification (no need of TreeMap)
      private val ranks = new ArrayBuffer[ArrayBuffer[Individual]]()

      class Individual private[IndividualCollection] (val handle: d.Individual) {
        // this is for comparison and rank determination
        private[IndividualCollection] val fitness = functions.map(handle.fitness)
        // this is for NSGA-II
        private[IndividualCollection] val publicObjectives = publicFunctions.map(handle.fitness)

        private[IndividualCollection] var rank: Int = 0
        private[IndividualCollection] var crowdingDistance: Double = 0

        def isDominatedBy(that: Individual): Boolean = dominates(that.publicObjectives, publicObjectives)
        def nObjectives: Int = publicObjectives.size + 1
        def objective(index: Int): Double = if (index == 0) indexTree.indexOf(this) else publicObjectives(index - 1)
      }

      def rankStats: String = ranks.zipWithIndex.map(p => (p._2, p._1.size)).mkString(", ")

      def last: Individual = indexTree.last

      def repopulateLocalQ(): Unit = {
        val discountFactor = 1 - 1.0 / budget
        localQ.clear()
        for (act <- acts) {
          val rawReward = indexTree.indexOf(new Individual(act.target)) - indexTree.indexOf(new Individual(act.source))
          localQ += (act.optimizer, act.firstObjective, rawReward - 1, discountFactor)
        }
      }

      def consume(handle: d.Individual): Individual = {
        val ind = new Individual(handle)
        if (indexTree.add(ind)) {
          // Reevaluate the NSGA-II ranks
          ranks.foreach(_.clear())

          if (ranks.isEmpty) {
            ranks += new ArrayBuffer[IndividualCollection.Individual]()
          }
          var index = indexTree.size - 1
          while (index >= 0) {
            val curr = indexTree.valueAt(index)
            val better = (index + 1 until indexTree.size).view.map(indexTree.valueAt).filter(curr.isDominatedBy).map(_.rank)
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
            for (obj <- 0 until indexTree.head.nObjectives) {
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
        indexTree.sameAs(ind)
      }

      def choose(): Individual = {
        val a, b = indexTree.valueAt(random.nextInt(indexTree.size))
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

    val globalExperienceDiscountFactor = 1 - 1.0 / (optimizers.size * functions.size)

    for (t <- 2 to budget) {
      IndividualCollection.repopulateLocalQ()

      val curr = IndividualCollection.choose().handle
      val globalMultiple = math.pow(globalExperienceDiscountFactor, t - 1)

      val candidatesWithWeights =
        optimizers.indices
          .flatMap(o => functions.indices.map(f => (o, f)))
          .map(p => (p._1, p._2, globalQ(p._1, p._2) * globalMultiple + localQ(p._1, p._2)))
          .filter(_._3 > 0)

      val (optimizerIndex, firstFunctionIndex) = if (candidatesWithWeights.isEmpty || random.nextInt(10) == 0) {
        (random.nextInt(optimizers.size), random.nextInt(functions.size))
      } else {
        var sample = random.nextDouble() * candidatesWithWeights.map(_._3).sum
        var index = 0
        while (index < candidatesWithWeights.size && sample >= candidatesWithWeights(index)._3) {
          sample -= candidatesWithWeights(index)._3
          index += 1
        }
        (candidatesWithWeights(index)._1, candidatesWithWeights(index)._2)
      }

      val optimizer = optimizers(optimizerIndex)
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
    outputDatabase.saveTo(outputFile)
  }

  def main(args: Array[String]): Unit = {
    val propertyFile = new File(args(0))
    val propertyParent = propertyFile.getParentFile.getAbsoluteFile
    val properties = new Properties()
    val stream = new FileInputStream(propertyFile)
    properties.load(stream)
    stream.close()

    val targetRoot = new File(propertyParent, properties.getProperty("target.root"))
    if (!targetRoot.exists() && !targetRoot.mkdirs()) {
      throw new IOException("Cannot create the root directory: '" + targetRoot.getAbsolutePath + "'")
    }
    val sourceRootCount = properties.getProperty("source.root.count", "0").toInt
    val databases = (0 until sourceRootCount).flatMap(i => RunDatabase.loadAll(properties.getProperty("source.root." + i)))
    val srv = VeeRouteService
    val summary = new PrintWriter(new File(propertyParent, properties.getProperty("summary")))
    val budget = properties.getProperty("budget").toInt
    try {
      for {
        idx <- properties.getProperty("idx.min").toInt to properties.getProperty("idx.max").toInt
        run <- 0 until properties.getProperty("runs").toInt
      } {
        srv.withDataset(srv.datasets(idx))(runOnDataset(databases, targetRoot, summary, budget, run)(srv))
      }
    } finally {
      summary.close()
    }
  }
}
