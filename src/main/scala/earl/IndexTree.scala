package earl

import java.util.Random

import scala.collection.immutable.VectorBuilder

class IndexTree[T](implicit ordering: Ordering[T]) {
  private[this] val random = new Random()
  private[this] def nextNonMaximumLong(): Long = {
    val rv = random.nextLong()
    if (rv == Long.MaxValue) nextNonMaximumLong() else rv
  }

  private[this] val splitPlace = Array.ofDim[Vertex](3)

  private[this] trait Vertex {
    def heap: Long
    def size: Int
    def valueAt(index: Int): T
    def indexOf(value: T, offset: Int): Int
    def addNode(node: Node): Vertex
    def remove(value: T): Vertex
    def merge(right: Vertex): Vertex
    def split(value: T, target: Array[Vertex]): Unit
    def head: T
    def last: T
    def sameAs(value: T): T
    def collect(builder: VectorBuilder[T]): Unit
  }

  private[this] case class Node(value: T, left: Vertex, right: Vertex, heap: Long) extends Vertex {
    override val size: Int = 1 + left.size + right.size

    override def valueAt(index: Int): T = {
      val ls = left.size
      if (index == ls) {
        value
      } else if (index < ls) {
        left.valueAt(index)
      } else {
        right.valueAt(index - ls - 1)
      }
    }

    override def indexOf(value: T, offset: Int): Int = {
      val ls = left.size
      val cmp = ordering.compare(value, this.value)
      if (cmp == 0) {
        offset + ls
      } else if (cmp < 0) {
        left.indexOf(value, offset)
      } else {
        right.indexOf(value, offset + ls + 1)
      }
    }

    override def addNode(node: Node): Vertex = {
      val cmp = ordering.compare(value, node.value)
      if (node.heap < heap) {
        split(node.value, splitPlace)
        assert(splitPlace(1) == Leaf)
        node.copy(left = splitPlace(0), right = splitPlace(2))
      } else {
        if (cmp < 0) {
          copy(right = right.addNode(node))
        } else {
          copy(left = left.addNode(node))
        }
      }
    }

    override def remove(value: T): Vertex = {
      val cmp = ordering.compare(value, this.value)
      if (cmp == 0) {
        left.merge(right)
      } else if (cmp < 0) {
        copy(left = left.remove(value))
      } else {
        copy(right = right.remove(value))
      }
    }

    override def merge(other: Vertex): Vertex = other match {
      case Leaf => this
      case that: Node =>
        if (heap < that.heap) {
          copy(right = right.merge(right))
        } else {
          that.copy(left = that.left.merge(this))
        }
    }

    override def split(value: T, target: Array[Vertex]): Unit = {
      val cmp = ordering.compare(value, this.value)
      if (cmp == 0) {
        target(0) = left
        target(1) = copy(left = Leaf, right = Leaf)
        target(2) = right
      } else if (cmp < 0) {
        left.split(value, target)
        target(2) = copy(left = target(2))
      } else {
        right.split(value, target)
        target(0) = copy(right = target(0))
      }
    }

    override def head: T = if (left.size == 0) value else left.head
    override def last: T = if (right.size == 0) value else right.last

    override def sameAs(value: T): T = {
      val cmp = ordering.compare(value, this.value)
      if (cmp == 0) {
        this.value
      } else if (cmp < 0) {
        left.sameAs(value)
      } else {
        right.sameAs(value)
      }
    }

    override def collect(builder: VectorBuilder[T]): Unit = {
      left.collect(builder)
      builder += value
      right.collect(builder)
    }
  }

  private[this] case object Leaf extends Vertex {
    override def size: Int = 0
    override def heap: Long = Long.MaxValue
    override def valueAt(index: Int): T = throw new IllegalArgumentException("Leaf.valueAt is undefined")
    override def indexOf(value: T, offset: Int): Int = -offset - 1
    override def addNode(node: Node): Vertex = node
    override def remove(value: T): Vertex = this
    override def merge(right: Vertex): Vertex = right
    override def split(value: T, target: Array[Vertex]): Unit = {
      target(0) = Leaf
      target(1) = Leaf
      target(2) = Leaf
    }
    override def head: T = throw new IllegalArgumentException("Leaf.head is undefined")
    override def last: T = throw new IllegalArgumentException("Leaf.last is undefined")
    override def sameAs(value: T): T = throw new IllegalArgumentException("Leaf.representer is undefined")
    override def collect(builder: VectorBuilder[T]): Unit = ()
  }

  private[this] var root: Vertex = Leaf

  def sameAs(value: T): T = {
    root.sameAs(value)
  }

  def add(value: T): Boolean = {
    indexOf(value) < 0 && {
      root = root.addNode(Node(value, Leaf, Leaf, nextNonMaximumLong()))
      true
    }
  }

  def remove(value: T): Boolean = {
    val newRoot = root.remove(value)
    val rv = newRoot eq root
    root = newRoot
    rv
  }

  def head: T = root.head
  def last: T = root.last
  def size: Int = root.size

  /**
    * Returns a non-negative index of the value, in the case the value is present.
    * Otherwise, returns `-x - 1` where `x` is the index which the value would get if it is inserted.
    * The semantics is the same as in `Arrays.binarySearch`.
    *
    * @param value the value for which the index needs to be found.
    * @return the actual index if non-negative, the hypothesized index if negative.
    */
  def indexOf(value: T): Int = root.indexOf(value, 0)

  def valueAt(index: Int): T = {
    if (index < 0 || index >= root.size) {
      throw new IndexOutOfBoundsException(s"Index $index, size ${root.size}")
    }
    root.valueAt(index)
  }

  def validate(): Unit = {
    val builder = new VectorBuilder[T]
    root.collect(builder)
    val result = builder.result()
    for (i <- 1 until result.size) {
      assert(ordering.compare(result(i - 1), result(i)) < 0)
    }
    for (i <- 0 until result.size) {
      assert(ordering.equiv(result(i), valueAt(i)))
      assert(i == indexOf(result(i)))
    }
  }
}
