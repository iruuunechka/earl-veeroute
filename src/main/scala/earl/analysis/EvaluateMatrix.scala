package earl.analysis

import earl.RunDatabase
import earl.reinforce.QMatrix

object EvaluateMatrix {
  def main(args: Array[String]): Unit = {
    val dbs = args.flatMap(RunDatabase.loadAll)
    println(s"[info] ${dbs.size} databases, ${dbs.iterator.map(_.acts.size).sum} acts in total")
    if (dbs.length > 0) {
      val example = dbs(0)
      val q = QMatrix.fromDatabases(dbs, example.objectives, example.optimizers)
      println(s"[info] Q-matrix has ${q.firstSize} rows/optimizers and ${q.secondSize} columns/functions")
      val allCells = for (o <- 0 until q.firstSize; f <- 0 until q.secondSize) yield (o, f)
      val argMax = allCells.maxBy(p => q(p._1, p._2))
      val argMaxV = q(argMax._1, argMax._2)
      val nArgMaxs = allCells.count(p => q(p._1, p._2) >= argMaxV - 1e-9)
      println(s"[info] arg max: count = $nArgMaxs value = $argMaxV")
      println(s"[info]    optimizer: ${example.optimizers(argMax._1)}")
      println(s"[info]    function:  ${example.objectives(argMax._2)}")
    }
  }
}
