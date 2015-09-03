package fp_in_scala.chapter_03

import scala.annotation.tailrec

/**
 * Created by jankeesvanandel on 31/08/15.
 */
object ListUtils {

  def tail[A](l: List[A]): Either[String, List[A]] = l match {
    case Nil => Left("Tail of an empty list!")
    case _ :: tail => Right(tail)
  }

  def setHead[A](l: List[A], newHead: A): Either[String, List[A]] = l match {
    case Nil => Left("Head of an empty list!")
    case head :: tail => Right(newHead :: tail)
  }

  def drop[A](l: List[A], n: Int): List[A] = l match {
    case _ :: tail if n > 0 => drop(tail, n-1)
    case _ => l
  }

  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case head :: tail if f(head) => dropWhile(tail, f)
    case _ => l
  }

  def init[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case _ :: Nil => Nil
    case head :: tail => head :: init(tail)
  }

  def foldRight[A, B](as: List[A], startValue: B)(f: (A, B) => B): B =
    as match {
      case Nil => startValue
      case head :: tail => f(head, foldRight(tail, startValue)(f))
    }

  def lengthUsingFoldRight[A](as: List[A]) =
    ListUtils.foldRight(as, 0)((_, b: Int) => b+1)

  @tailrec
  def foldLeft[A, B](as: List[A], startValue: B)(f: (B, A) => B): B = {
    as match {
      case Nil => startValue
      case head :: tail => foldLeft(tail, f(startValue, head))(f)
    }
  }

}
