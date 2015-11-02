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
