package fp_in_scala.chapter_04

sealed trait Option[+A] {
  def map[B](fn: A => B): Option[B] = this match {
    case Some(value) => Some(fn(value))
    case None => None
  }

  def flatMap[B](fn: A => Option[B]): Option[B] =
    this.map(fn).getOrElse(None)

  def getOrElse[B >: A](default: => B): B = this match {
    case Some(value) => value
    case None => default
  }

  def orElse[B >: A](default: => Option[B]): Option[B] = this match {
    case some@Some(_) => some
    case None => default
  }


  def filter(fn: A => Boolean): Option[A] =
    if (this.map(fn).getOrElse(false)) this
    else None

}

case class Some[+A](get: A) extends Option[A]

case object None extends Option[Nothing]

object Option {

  def mean(xs: Seq[Double]): Option[Double] = {
    if (xs.isEmpty) None
    else Some(xs.sum / xs.size)
  }

  def variance(xs: Seq[Double]): Option[Double] = {
    mean(xs).flatMap { m =>
      mean(xs.map{ x =>
        math.pow(x - m, 2)
      })
    }
  }

  def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = {
    for {
      aa <- a
      bb <- b
    } yield {
      f(aa, bb)
    }
  }

  def sequence[A](a: List[Option[A]]): Option[List[A]] = {
    a.foldRight[Option[List[A]]](Some(Nil))(map2(_, _)(_ :: _))
  }

  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = {
    a.foldRight[Option[List[B]]](Some(Nil))((h, t) => map2(f(h), t)(_ :: _))
  }

  def sequenceInTermsOfTraverse[A](a: List[Option[A]]): Option[List[A]] = {
    traverse(a)(o => o)
  }
}