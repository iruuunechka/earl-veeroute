package earl.analysis

import java.io.PrintWriter
import java.util.Locale

import earl.RunDatabase
import earl.util.Relations

object BasicResults {
  class Stats(val run: RunDatabase, val index: Int) {
    val sortedRuns: Seq[Seq[Double]] = run.individuals.distinct.sortWith((l, r) => Relations.compareSeq(l, r) < 0)
    val first: Seq[Double] = run.individuals.head
    val best: Seq[Double] = sortedRuns.last
    val worst: Seq[Double] = sortedRuns.head

    val (bestDistance, optCountForBest, objCountForBest) = {
      val inf = run.individuals.size + 1
      val dist = Array.fill(run.individuals.size)(inf)
      val optMask, objMask = Array.fill(run.individuals.size)(Int.MaxValue)
      dist(0) = 0
      optMask(0) = 0
      objMask(0) = 0
      for (a <- run.acts) {
        assert(dist(a.source) != inf)
        if (dist(a.target) > dist(a.source) + 1) {
          dist(a.target) = dist(a.source) + 1
          optMask(a.target) = Int.MaxValue
          objMask(a.target) = Int.MaxValue
        }
        if (dist(a.target) == dist(a.source) + 1) {
          val newOptMask = optMask(a.source) | (1 << a.optimizer)
          val newObjMask = objMask(a.source) | (1 << a.firstObjective)
          if (Integer.bitCount(optMask(a.target)) > Integer.bitCount(newOptMask)) {
            optMask(a.target) = newOptMask
          }
          if (Integer.bitCount(objMask(a.target)) > Integer.bitCount(newObjMask)) {
            objMask(a.target) = newObjMask
          }
        }
        dist(a.target) = math.min(dist(a.target), dist(a.source) + 1)
      }
      val optima = dist.indices.filter(i => run.individuals(i) == best)
      (optima.map(dist).min,
        optima.map(o => Integer.bitCount(optMask(o))).min,
        optima.map(o => Integer.bitCount(objMask(o))).min)
    }
  }

  def numberToLaTeX(value: Double): String = {
    val str = "%.4g".formatLocal(Locale.US, value)
    val e = str.indexOf('e')
    if (e == -1) {
      s"$$$str$$"
    } else {
      s"$$${str.substring(0, e)} \\cdot 10^{${str.substring(e + 1).toInt}}$$"
    }
  }

  def main(args: Array[String]): Unit = {
    val dbs = args.flatMap(RunDatabase.loadAll)
    val tables = new PrintWriter("/home/maxbuzz/repos/itmo/genome-work/ai-papers/aspirants/petrova-i-a/thesis/dissertation/interning-tables.tex")

    class Stat {
      private var sum, max, count = 0
      private var min = Int.MaxValue
      def += (v: Int): Unit = {
        count += 1
        sum += v
        max = math.max(max, v)
        min = math.min(min, v)
      }
      def get: String = s"$min, ${sum.toDouble / count}, $max"
    }

    val sortedRuns, bestDistances, objCounts, optCounts = new Stat

    for ((problem, runs) <- dbs.groupBy(_.problemName).toIndexedSeq.sortBy(_._1)) {
      val problemNoExt = problem.substring(0, problem.indexOf('.'))
      println(s"$problem: ${runs.size} runs")
      val theRuns = runs.zipWithIndex.map(p => new Stats(p._1, p._2))
      val startIsSame = theRuns.map(_.first).distinct.length == 1
      if (!startIsSame) {
        println("*********************************************")
        println("* S T A R T   I S   N O T   T H E   S A M E *")
        println("*********************************************")
      }
      tables.println(s"\\section{Набор данных \\texttt{${
        problemNoExt.replace("_", "\\_")
      }}}\\label{full-notl-${
        problemNoExt.replace('_', '-')
      }}")
      tables.println("\\begingroup\\centering\\scriptsize")
      tables.println("\\noindent\\begin{tabular}{|p{4cm}|c|c|c|c|c|c|}")
      theRuns foreach { r =>
        tables.println(s"\\hline\\multicolumn{7}{|l|}{Запуск №${
          r.index + 1
        }, различных расписаний ${
          r.sortedRuns.size
        }, от первого до лучшего: расстояние ${
          r.bestDistance
        }, оптимизаторов ${r.optCountForBest}, критериев ${r.objCountForBest}}\\\\")

        sortedRuns += r.sortedRuns.size
        bestDistances += r.bestDistance
        objCounts += r.objCountForBest
        optCounts += r.optCountForBest

        def printOne(seq: Seq[Double], name: String): Unit = {
          val idx = r.sortedRuns.indexOf(seq)
          tables.println(seq.take(5).map(numberToLaTeX).mkString(s"\\hline $name & Номер ${idx + 1} & ", " & ", " \\\\"))
        }

        printOne(r.first, "Первое")
        printOne(r.run.individuals(1), "Второе")
        printOne(r.best, "Лучшее")
      }
      tables.println("\\hline\\end{tabular}")
      tables.println("\\par\\endgroup")
      tables.println()
    }
    tables.close()

    println(s"sortedRuns size: ${sortedRuns.get}")
    println(s"bestDistance: ${bestDistances.get}")
    println(s"optimizer counts: ${optCounts.get}")
    println(s"objective counts: ${objCounts.get}")
  }
}
