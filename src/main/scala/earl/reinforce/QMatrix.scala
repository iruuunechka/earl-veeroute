package earl.reinforce

import earl.RunDatabase
import earl.util.Relations.compareSeq

class QMatrix(val firstSize: Int, val secondSize: Int) {
  private[this] val cells = Array.fill(firstSize, secondSize)(Double.NaN: Double)
  private[this] val countOfFirst = Array.fill(firstSize)(0)
  private[this] val sumOfFirst = Array.fill(firstSize)(0.0)
  private[this] val countOfSecond = Array.fill(secondSize)(0)
  private[this] val sumOfSecond = Array.fill(secondSize)(0.0)

  def += (first: Int, second: Int, reward: Double, prevMultiple: Double): Unit = {
    if (cells(first)(second).isNaN) {
      cells(first)(second) = reward
    } else {
      cells(first)(second) = prevMultiple * cells(first)(second) + reward
    }
    sumOfFirst(first) += reward
    sumOfSecond(second) += reward
    countOfFirst(first) += 1
    countOfSecond(first) += 1
  }

  def apply(first: Int, second: Int): Double = {
    if (cells(first)(second).isNaN) {
      if (countOfFirst(first) != 0 && countOfSecond(second) != 0) {
        0.5 * (sumOfFirst(first) / countOfFirst(first) + sumOfSecond(second) / countOfSecond(second))
      } else if (countOfFirst(first) != 0) {
        sumOfFirst(first) / countOfFirst(first)
      } else if (countOfSecond(second) != 0) {
        sumOfSecond(second) / countOfSecond(second)
      } else 0.0
    } else {
      cells(first)(second)
    }
  }

  def clear(): Unit = {
    import java.util.Arrays.fill
    fill(countOfFirst, 0)
    fill(countOfSecond, 0)
    fill(sumOfFirst, 0.0)
    fill(sumOfSecond, 0.0)
    for (c <- cells) {
      fill(c, Double.NaN)
    }
  }
}

object QMatrix {
  def fromDatabases(databases: Seq[RunDatabase], functionNames: Seq[String], optimizerNames: Seq[String]): QMatrix = {
    val q = new QMatrix(optimizerNames.size, functionNames.size)
    for (db <- databases) {
      val objectiveReindex = db.objectives.map(name => functionNames.zipWithIndex.find(_._1 == name).map(_._2).getOrElse(-1))
      val optimizerReindex = db.optimizers.map(name => optimizerNames.zipWithIndex.find(_._1 == name).map(_._2).getOrElse(-1))
      val individualsRemap = db.individuals.map(ind => functionNames.indices.map(i => if (objectiveReindex(i) == -1) 0.0 else ind(objectiveReindex(i))))
      val indicesSorted = individualsRemap.indices.sortWith((l, r) => compareSeq(individualsRemap(l), individualsRemap(r)) < 0)

      for (act <- db.acts) {
        val actualObjective = objectiveReindex(act.firstObjective)
        val actualOptimizer = optimizerReindex(act.optimizer)
        val actualSource = indicesSorted.indexOf(act.source)
        val actualTarget = indicesSorted.indexOf(act.target)
        q += (actualOptimizer, actualObjective, actualTarget - actualSource, 1)
      }
    }
    q
  }
}
