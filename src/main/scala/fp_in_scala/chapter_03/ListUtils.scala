package fp_in_scala.chapter_03

import scala.annotation.tailrec
import scala.collection.immutable

object ListUtils {
  def setHead[A](as: List[A])(newHead: A): List[A] = as match {
    case Nil => Nil
    case Cons(head, tail) => Cons(newHead, tail)
  }

  implicit def fromScala[A](scalaList: scala.List[A]): List[A] = {
    @tailrec
    def doFromScala(acc: List[A], scalaList: scala.List[A]): List[A] = {
      scalaList match {
        case immutable.Nil => acc
        case head :: tail => doFromScala(Cons(head, acc), tail)
      }
    }
    doFromScala(Nil, scalaList.reverse)
  }


  implicit def toScala[A](ownList: List[A]): scala.List[A] = {
    @tailrec
    def doToScala(acc: scala.List[A], ownList: List[A]): scala.List[A] = {
      ownList match {
        case Nil => acc
        case Cons(head, tail) => doToScala(head :: acc, tail)
      }
    }
    doToScala(scala.List.empty, ownList).reverse
  }

  def sumUsingFoldLeft(numbers: List[Int]): Int = numbers.foldLeft(0)(_ + _)

  def productUsingFoldLeft(numbers: List[Int]): Int = numbers.foldLeft(1)(_ * _)

  def lengthUsingFoldLeft[A](numbers: List[A]): Int = numbers.foldLeft(0)((b, a) => b+1)
  def reverseUsingFoldLeft[A](list: List[A]): List[A] = list.foldLeft(Nil:List[A])((b: List[A], a: A) => Cons(a, b))

  def foldRightUsingFoldLeft[A, B](list: List[A], acc: B)(f: (A, B) => B): B = {
    reverseUsingFoldLeft(list).foldLeft(acc)((acc: B, a: A) => f(a, acc))
  }

  def foldLeftUsingFoldRight[A, B](list: List[A], acc: B)(f: (B, A) => B): B = {
    list.foldRight(acc)((a: A, acc: B) => f(acc, a))
  }

  def appendUsingFoldLeft[A](list: List[A], other: List[A]): List[A] = {
    reverseUsingFoldLeft(list).foldLeft(other)((b, a) => Cons(a, b))
  }

  def appendUsingFoldRight[A](list: List[A], other: List[A]): List[A] = {
    foldRightUsingFoldLeft(list, other)((a, b) => Cons(a, b))
  }

  def concatenateListOfLists[A](list: List[List[A]]): List[A] = {
    val concatenated = list.foldLeft(Nil: List[A]) {
      (acc: List[A], a: List[A]) =>
        a.foldLeft(acc)((b, a) => Cons(a, b))
    }
    reverseUsingFoldLeft(concatenated)
  }

  def addTwoIntLists(list: List[Int], other: List[Int]): List[Int] = {
    @tailrec
    def doAddTwoLists(acc: List[Int], list: List[Int], other: List[Int]): List[Int] = list match {
      case Cons(head1, cons1) => other match {
        case Cons(head2, cons2) =>
          doAddTwoLists(Cons(head1 + head2, acc), cons1, cons2)
        case Nil =>
          sys.error("Lists not of the same size")
      }
      case Nil if other.isEmpty => acc
      case Nil => sys.error("Lists not of the same size")
    }
    ListUtils.reverseUsingFoldLeft(doAddTwoLists(Nil, list, other))
  }

  def hasSubsequence[A](list: List[A], subsequence: List[A]): Boolean = {
    def startsWithSubsequence(list: List[A], subsequence: List[A]): Boolean = list match {
      case Nil => subsequence.isEmpty
      case Cons(head, tail) if subsequence.isEmpty => true
      case Cons(head, tail) if head != subsequence.head => false
      case Cons(head, tail) => startsWithSubsequence(tail, subsequence.tail)
    }
    if (subsequence.isEmpty) true
    else {
      list match {
        case Nil => false
        case Cons(head, tail) if startsWithSubsequence(list, subsequence) => true
        case Cons(head, tail) => hasSubsequence(tail, subsequence) // Continue with next
      }
    }
  }
}
