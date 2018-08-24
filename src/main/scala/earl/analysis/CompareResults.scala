package earl.analysis

import java.nio.file.Files

import scala.collection.JavaConverters._

import earl.RunDatabase
import earl.util.Relations

object CompareResults {
  private[this] implicit val seqOrdering: Ordering[Seq[Double]] = Relations.compareSeq

  def runRscript(s: String): Unit = {
    val theScript = s"""#!/usr/bin/env Rscript
      |options(echo = FALSE)
      |args <- commandArgs(trailingOnly = TRUE)
      |
      |v1 <- ${s.indices.filter(i => s(i) == 'L').mkString("c(", ",", ")")}
      |v2 <- ${s.indices.filter(i => s(i) == 'R').mkString("c(", ",", ")")}
      |
      |pTwo <- stats::wilcox.test(v1, v2, alternative="two.sided")$$p.value
      |pLess <- stats::wilcox.test(v1, v2, alternative="less")$$p.value
      |pGreater <- stats::wilcox.test(v1, v2, alternative="greater")$$p.value
      |
      |fsymb <- ifelse(pTwo < 0.05, ifelse(pLess < 0.05, "[<]", "[>]"), "[?]")
      |
      |cat(paste(fsymb, " { <", pLess, "} { =", pTwo, "} { >", pGreater, "}\n"))
    """.stripMargin
    val tmpFile = Files.createTempFile("earlVeeRoute", ".R")
    Files.write(tmpFile, Seq(theScript).asJava)
    new ProcessBuilder("Rscript", tmpFile.toString).inheritIO().start().waitFor()
  }

  def main(args: Array[String]): Unit = {
    if (args.length != 2) {
      System.err.println("Usage: CompareResults <left-root> <right-root>")
    } else {
      val left = RunDatabase.loadAll(args(0)).groupBy(_.problemName)
      val right = RunDatabase.loadAll(args(1)).groupBy(_.problemName)
      println(s"Left: ${left.size} databases, right: ${right.size} databases")
      val keys = left.keySet ++ right.keySet
      for (k <- keys) {
        val lBest = left(k).map(db => db.individuals.min -> "L")
        val rBest = right(k).map(db => db.individuals.min -> "R")
        val orderOfBestIndividuals = (lBest ++ rBest).sortBy(_._1).map(_._2).mkString("")
        print(s"$k: $orderOfBestIndividuals ")
        runRscript(orderOfBestIndividuals)
      }
    }
  }
}
