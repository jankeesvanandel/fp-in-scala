package fp_in_scala.chapter_03

import scala.annotation.tailrec

sealed trait List[+A] {

  implicit def stringToRuntimeException(string: String): RuntimeException = {
    new RuntimeException(string)
  }

  def head: A
  def tail: List[A]
  def isEmpty: Boolean

  def drop(n: Int): List[A] = this match {
    case Cons(_, t) if n > 0 => t.drop(n - 1)
    case _ => this
  }

  @tailrec
  final def dropWhile(f: A => Boolean): List[A] = this match {
    case Cons(h, t) if f(h) => t.dropWhile(f)
    case _ => this
  }

  def init: List[A] = this match {
    case Nil => Nil
    case Cons(h, Nil) => Nil
    case Cons(h, t) => Cons(h, t.init)
  }

  def foldRight[B](startValue: B)(f: (A, B) => B): B =
    this match {
      case Nil => startValue
      case Cons(h, t) => f(h, t.foldRight(startValue)(f))
    }

  def lengthUsingFoldRight = foldRight(0)((_, b: Int) => b + 1)

  @tailrec
  final def foldLeft[B](startValue: B)(f: (B, A) => B): B = this match {
    case Nil => startValue
    case Cons(h, t) => t.foldLeft(f(startValue, h))(f)
  }

  def map[B](fn: (A) => B): List[B] = this match {
    case Cons(h, t) => Cons(fn(h), t.map(fn))
    case Nil => Nil
  }

  def filter(fn: (A) => Boolean): List[A] = {
    ListUtils.foldRightUsingFoldLeft(this, Nil: List[A]) { (a: A, acc: List[A]) =>
      if (fn(a)) Cons(a, acc)
      else acc
    }
  }

  def flatMap[B](fn: (A) => List[B]): List[B] = {
    ListUtils.foldRightUsingFoldLeft(this, Nil: List[B]) { (a: A, acc: List[B]) =>
      ListUtils.appendUsingFoldLeft(fn(a), acc)
    }
  }

  def filterUsingFlatMap(fn: (A) => Boolean): List[A] = {
    this.flatMap[A] { (a) =>
      if (fn(a)) Cons(a, Nil)
      else Nil
    }
  }

  def zipWith[B, C](other: List[B])(combineFn: (A, B) => C): List[C] = {
    @tailrec
    def doZipTwoLists(acc: List[C], list: List[A], other: List[B]): List[C] = list match {
      case Cons(head1, cons1) => other match {
        case Cons(head2, cons2) =>
          doZipTwoLists(Cons(combineFn(head1, head2), acc), cons1, cons2)
        case Nil =>
          sys.error("Lists not of the same size")
      }
      case Nil if other.isEmpty => acc
      case Nil => sys.error("Lists not of the same size")
    }
    ListUtils.reverseUsingFoldLeft(doZipTwoLists(Nil, this, other))
  }

  override def toString: String = {
    @tailrec
    def doToString(acc: String, list: List[A]): String = list match {
      case Nil => acc
      case Cons(head, tail) => doToString(acc + "," + head, tail)
    }
    this match {
      case Nil => "List()"
      case Cons(head, tail) => doToString("List(" + head, tail) + ")"
    }
  }
}

object List {
  def apply[A](as: A*): List[A] =
    ListUtils.fromScala(as.toList)
  def range(startIncl: Int, endExcl: Int, step: Int = 1): List[Int] =
    ListUtils.fromScala(scala.List.range(startIncl, endExcl, step))
}

case object Nil extends List[Nothing] {
  def head = sys.error("Head of an empty list")
  def tail = sys.error("Tail of an empty list")
  val isEmpty = true
}

case class Cons[+A](head: A, tail: List[A]) extends List[A] {
  val isEmpty = false
}