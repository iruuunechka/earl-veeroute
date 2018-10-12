package earl.analysis

import java.util.Arrays

import scala.collection.mutable.{HashMap => MuHashMap}

import earl.RunDatabase
import earl.reinforce.QMatrix

object EvaluateMatrix {
  def main(args: Array[String]): Unit = {
    val reward: (Int, Int) => Double = args(0) match {
      case "diff" => (s, t) => t - s
      case "target" => (_, t) => t
      case v => throw new IllegalArgumentException("Unknown reward function: " + v)
    }
    val dbs = args.tail.flatMap(RunDatabase.loadAll)
    println(s"[info] ${dbs.length} databases, ${dbs.iterator.map(_.acts.size).sum} acts in total")
    if (dbs.length > 0) {
      val example = dbs(0)
      val allCells = for (o <- example.optimizers.indices; f <- example.objectives.indices) yield (o, f)
      val cellSumRank = new MuHashMap[(Int, Int), Int]
      for (db <- dbs) {
        val q = QMatrix.fromDatabases(Seq(db), example.objectives, example.optimizers, reward)
        val sortedCells = allCells.sortBy(p => -q.apply(p._1, p._2))
        sortedCells.zipWithIndex.foreach(p => cellSumRank.update(p._1, cellSumRank.getOrElse(p._1, 0) + p._2))
      }
      val hittingProbabilities = {
        val curr, next = Array.ofDim[Double](dbs.length * (allCells.size - 1) + 1)
        curr(0) = 1
        val acl = allCells.size
        for (_ <- dbs.indices) {
          var i = curr.length - 1
          Arrays.fill(next, 0.0)
          while (i >= 0) {
            if (curr(i) > 0) {
              var j = 0
              val delta = curr(i) / acl
              while (j < acl) {
                next(i + j) += delta
                j += 1
              }
            }
            i -= 1
          }
          assert(math.abs(next.sum - 1) < 1e-9)
          System.arraycopy(next, 0, curr, 0, curr.length)
        }
        curr
      }
      val prefixes = hittingProbabilities.clone()
      for (i <- 1 until prefixes.length) prefixes(i) += prefixes(i - 1)

      cellSumRank.toIndexedSeq.sortBy(_._2).foreach { case ((opt, obj), rankSum) =>
        val raw = "%.04e".format(prefixes(rankSum))
        val bnf = "%.04e".format(math.min(1, prefixes(rankSum) * cellSumRank.size))
        println(s"$rankSum [p = $raw raw / $bnf bonf'd] => ${example.optimizers(opt)}, ${example.objectives(obj)}")
      }
    }
  }
}
