package earl.analysis

import scala.collection.mutable.{HashMap => MuHashMap}

import earl.RunDatabase
import earl.reinforce.QMatrix

object EvaluateMatrix {
  def main(args: Array[String]): Unit = {
    val dbs = args.flatMap(RunDatabase.loadAll)
    println(s"[info] ${dbs.size} databases, ${dbs.iterator.map(_.acts.size).sum} acts in total")
    if (dbs.length > 0) {
      val example = dbs(0)
      val allCells = for (o <- example.optimizers.indices; f <- example.objectives.indices) yield (o, f)
      val cellSumRank = new MuHashMap[(Int, Int), Int]
      for (db <- dbs) {
        val q = QMatrix.fromDatabases(Seq(db), example.objectives, example.optimizers)
        val sortedCells = allCells.sortBy(p => -q.apply(p._1, p._2))
        sortedCells.zipWithIndex.foreach(p => cellSumRank.update(p._1, cellSumRank.getOrElse(p._1, 0) + p._2))
      }
      println(s"Average sum-rank: ${dbs.size * allCells.size / 2}")
      cellSumRank.toIndexedSeq.sortBy(_._2).foreach(p => println(s"${p._2} => ${example.optimizers(p._1._1)}, ${example.objectives(p._1._2)}"))
    }
  }
}
