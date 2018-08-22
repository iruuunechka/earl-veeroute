package earl.util

import scala.annotation.tailrec

object Relations {
  private[this] val toleranceValue = 1e-4

  def tolerance: Double = toleranceValue

  def compareSeq(l: Seq[Double], r: Seq[Double]): Int = compareSeqImpl(l, r, 0)
  def dominates(l: Seq[Double], r: Seq[Double]): Boolean = dominatesImpl(l, r, 0)

  @tailrec
  private[this] def compareSeqImpl(l: Seq[Double], r: Seq[Double], i: Int): Int = {
    if (i == l.size) {
      0
    } else if (l(i) + tolerance < r(i)) {
      1
    } else if (l(i) > tolerance + r(i)) {
      -1
    } else compareSeqImpl(l, r, i + 1)
  }

  @tailrec
  private[this] def dominatesImpl(l: Seq[Double], r: Seq[Double], i: Int): Boolean = {
    if (i == l.size) {
      true
    } else if (l(i) > tolerance + r(i)) {
      false
    } else dominatesImpl(l, r, i + 1)
  }
}
