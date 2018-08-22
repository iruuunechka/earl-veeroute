package earl.reinforce

class QMatrix(firstSize: Int, secondSize: Int) {
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
