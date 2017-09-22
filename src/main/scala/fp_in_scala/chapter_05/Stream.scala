package fp_in_scala.chapter_05

import fp_in_scala.chapter_03._
import fp_in_scala.chapter_04._

sealed trait Stream[+A] {

  def headOption: Option[A] = foldRight(Option.empty[A]) { // Using foldRight
    case (a, _) => Some(a)
  }
  //  def headOption: Option[A] = this match {
  //    case EmptyStream => None
  //    case ConsStream(h, _) => Some(h())
  //  }

  def isEmpty: Boolean = this match { // Plain implementation
    case EmptyStream => true
    case ConsStream(_, _) => false
  }

  def toList: List[A] = this match { // Plain implementation
    case EmptyStream => Nil
    case ConsStream(h, t) => Cons(h(), t().toList)
  }

  def take(n: Int): Stream[A] = Stream.unfold((this, n)) { // Using unfold
    case (EmptyStream, _) => None
    case (_, s) if s <= 0 => None
    case (ConsStream(h, t), s) => Some((h(), (t(), s - 1)))
  }
  //  def take(n: Int): Stream[A] = this match { // Plain implementation
  //    case _ if n <= 0 => EmptyStream
  //    case EmptyStream => EmptyStream
  //    case ConsStream(h, t) =>
  //      Stream.cons(h(), t().take(n - 1))
  //  }

  def takeWhile(p: A => Boolean): Stream[A] = Stream.unfold(this) { // Using unfold
    case ConsStream(h, t) if p(h()) => Some((h(), t()))
    case _ => None
  }
  //  def takeWhile(p: A => Boolean): Stream[A] = foldRight(Stream.empty[A])((a, b) => // Using foldRight
  //    if (p(a)) ConsStream(() => a, () => b)
  //    else EmptyStream
  //  )
  //  def takeWhile(p: A => Boolean): Stream[A] = this match { // Plain implementation
  //    case EmptyStream => EmptyStream
  //    case ConsStream(h, t) if p(h()) =>
  //      ConsStream(() => h(), () => t().takeWhile(p))
  //    case _ => EmptyStream
  //  }

  def drop(n: Int): Stream[A] = this match { // Plain implementation
    case EmptyStream => EmptyStream
    case ConsStream(_, t) if n > 0 => t().drop(n - 1)
    case c@ConsStream(_, _) => c
  }

  def dropWhile(p: A => Boolean): Stream[A] = this match { // Plain implementation
    case EmptyStream => EmptyStream
    case ConsStream(h, t) if p(h()) => t().dropWhile(p)
    case c@ConsStream(_, _) => c
  }

  def foldRight[B](z: => B)(f: (A, => B) => B): B = this match { // Plain implementation
    case ConsStream(h, t) => f(h(), t().foldRight(z)(f))
    case _ => z
  }

  def exists(p: A => Boolean): Boolean = foldRight(false)((a, b) => p(a) || b) // Using foldRight
  //  def exists(p: A => Boolean): Boolean = this match { // Plain implementation
  //    case EmptyStream => false
  //    case ConsStream(h, t) => p(h()) || t().exists(p)
  //  }

  //  def contains(a: A): Boolean = exists(_ == a)

  def forAll(p: A => Boolean): Boolean = foldRight(true)((a, b) => p(a) && b) // Using foldRight

  def append[B >: A](other: => Stream[B]): Stream[B] = foldRight(other)(Stream.cons(_, _)) // Using foldRight
  //  def append[B >: A](other: => Stream[B]): Stream[B] = this match { // Plain implementation
  //    case EmptyStream => other
  //    case ConsStream(h, t) =>
  //      Stream.cons(h(), t().append(other))
  //  }

  def map[B](fn: A => B): Stream[B] = Stream.unfold(this) { // Using unfold
    case ConsStream(h, t) => Some((fn(h()), t()))
    case _ => None
  }
  //  def map[B](fn: A => B): Stream[B] = foldRight(Stream.empty[B])((a, b) => Stream.cons(fn(a), b)) // Using foldRight
  //  def map[B](fn: A => B): Stream[B] = this match { // Plain implementation
  //    case EmptyStream => EmptyStream
  //    case ConsStream(h, t) =>
  //      Stream.cons(fn(h()), t().map(fn))
  //  }

  def flatMap[B](fn: A => Stream[B]): Stream[B] = foldRight(Stream.empty[B])((a, b) => fn(a).append(b)) // Using foldRight
  //  def flatMap[B](fn: A => Stream[B]): Stream[B] = this match { // Plain implementation
  //    case EmptyStream => EmptyStream
  //    case ConsStream(h, t) =>
  //      fn(h()).append(t().flatMap(fn))
  //  }

  def filter(p: A => Boolean): Stream[A] = foldRight(Stream.empty[A]) { (a, b) => // Using foldRight
    if (p(a)) Stream.cons(a, b) else b
  }
  //  def filter(p: A => Boolean): Stream[A] = this match { // Plain implementation
  //    case EmptyStream => EmptyStream
  //    case ConsStream(h, t) =>
  //      val tl = t().filter(p)
  //      if (p(h())) Stream.cons(h(), tl)
  //      else tl
  //  }

  def zipWith[B, C](s2: Stream[B])(f: (A, B) => C): Stream[C] = Stream.unfold((this, s2)) { // Using unfold
    case (ConsStream(h1, t1), ConsStream(h2, t2)) => Some((f(h1(), h2()), (t1(), t2())))
    case _ => None
  }

  def zipAll[B](s2: Stream[B]): Stream[(Option[A], Option[B])] = Stream.unfold((this, s2)) { // Using unfold
    case (EmptyStream, EmptyStream) => None
    case (EmptyStream, ConsStream(h, t)) => Some(((None, Some(h())), (EmptyStream, t())))
    case (ConsStream(h, t), EmptyStream) => Some(((Some(h()), None), (t(), EmptyStream)))
    case (ConsStream(h1, t1), ConsStream(h2, t2)) => Some(((Some(h1()), Some(h2())), (t1(), t2())))
  }

  def startsWith[A](s2: Stream[A]): Boolean =
    zipAll(s2)
      .takeWhile { case (_, s2Opt) => s2Opt.nonEmpty }
      .forAll { case (a1Opt, a2Opt) => a1Opt == a2Opt }

  def tails: Stream[Stream[A]] = Stream.unfold(this) { // Using unfold
    case c: ConsStream[A] =>
      Some((c, c.t()))
    case _ => None
  }

  def hasSubsequence[A](s2: Stream[A]): Boolean = tails.exists(_.startsWith(s2))

  def scanRight[B](z: => B)(f: (A, => B) => B): Stream[B] = foldRight((z, Stream(z))) { (a, b) => // Using unfold
    lazy val b1 = b
    val b2 = f(a, b1._1)
    (b2, Stream.cons(b2, b1._2))
  }._2
}

case object EmptyStream extends Stream[Nothing] {
  override def equals(obj: scala.Any): Boolean = obj.isInstanceOf[EmptyStream.type]

}
case class ConsStream[+A](h: () => A, t: () => Stream[A]) extends Stream[A] {

  override def toString = s"${this.getClass.getSimpleName}(${h()}, ${t().toString})"

  override def equals(obj: scala.Any): Boolean = {
    if (obj.isInstanceOf[EmptyStream.type]) {
      false
    } else if (obj.isInstanceOf[ConsStream[A]]) {
      val cs = obj.asInstanceOf[ConsStream[A]]
      if (cs.h() != this.h()) false
      else cs.t().equals(this.t())
    } else {
      false
    }
  }

}

object Stream {

  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    ConsStream(() => head, () => tail)
  }

  def empty[A]: Stream[A] = EmptyStream

  def apply[A](as: A*): Stream[A] = {
    if (as.isEmpty) EmptyStream else cons(as.head, apply(as.tail: _*))
  }

  def ones: Stream[Int] = unfold(Unit)(_ => Some((1, Unit)))
//  def ones: Stream[Int] = constant(1)

  def constant[A](a: A): Stream[A] = unfold(Unit)(_ => Some((a, Unit)))
//  def constant[A](a: A): Stream[A] = {
//    lazy val as: Stream[A] = Stream.cons(a, as)
//    as
//  }

  def from(n: Int): Stream[Int] = unfold(n)(s => Some((s, s+1)))
//  def from(n: Int): Stream[Int] = {
//    Stream.cons(n, from(n+1))
//  }

  def fibs: Stream[Int] = Stream(0).append(unfold((0, 1))(s => {
    val newVal = s._1 + s._2
    val newState = (newVal, s._1)
    Some((newVal, newState))
  }))
//  def fibs: Stream[Int] = {
//    def doFib(n: Int, n1: Int): Stream[Int] = {
//      val newVal = n + n1
//      Stream.cons(newVal, doFib(n1, newVal))
//    }
//    Stream(0, 1).append(doFib(0, 1))
//  }

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = {
    val asOpt: Option[(A, S)] = f(z)
    asOpt match {
      case None => Stream.empty
      case Some((a, s)) => Stream.cons(a, unfold(s)(f))
    }
  }
}
