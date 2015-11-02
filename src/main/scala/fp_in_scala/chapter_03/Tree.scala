package fp_in_scala.chapter_03

sealed trait Tree[+A] {
  def size: Int
  def depth: Int
  def map[B](fn: (A) => B): Tree[B]
  def fold[B](fnLeaf: (A) => B)(fnBranch: (B, B) => B): B
}

case class Leaf[A](value: A) extends Tree[A] {
  def size: Int = 1
  def depth: Int = 1
  def map[B](fn: (A) => B): Leaf[B] = Leaf(fn(value))
  def fold[B](fnLeaf: (A) => B)(fnBranch: (B, B) => B): B = {
    fnLeaf(value)
  }
}
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A] {
  def size: Int = {
    1 + left.size + right.size
  }

  def depth: Int = {
    1 + (left.depth max right.depth)
  }

  def map[B](fn: (A) => B): Branch[B] = Branch(left.map(fn), right.map(fn))

  def fold[B](fnLeaf: (A) => B)(fnBranch: (B, B) => B): B = {
    fnBranch(
      left.fold(fnLeaf)(fnBranch),
      right.fold(fnLeaf)(fnBranch)
    )
  }
}

object Tree {
  def maximum(tree: Tree[Int]): Int = {
    def doMax(maxCounter: Int, tree: Tree[Int]): Int = tree match {
      case Leaf(value) => maxCounter max value
      case Branch(left, right) =>
        doMax(maxCounter, left) max doMax(maxCounter, right)
    }
    doMax(Integer.MIN_VALUE, tree)
  }

  def sizeWithFold[A](tree: Tree[A]): Int = {
    tree.fold(a => 1)((l, r) => 1 + l + r)
  }

  def depthWithFold[A](tree: Tree[A]): Int = {
    tree.fold(a => 1)((l, r) => 1 + (l max r))
  }

  def maximumWithFold(tree: Tree[Int]): Int = {
    tree.fold(identity)((l, r) => l max r)
  }

  def mapWithFold[A, B](tree: Tree[A])(fn: (A) => B): Tree[B] = {
    tree.fold(a => Leaf(fn(a)): Tree[B])((l, r) => Branch(l, r))
  }
}