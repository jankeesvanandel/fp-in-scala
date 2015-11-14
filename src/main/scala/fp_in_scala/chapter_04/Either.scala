package fp_in_scala.chapter_04

/**
  * Created by jankeesvanandel on 14/11/15.
  */
sealed trait Either[+E, +A] {
  def map[B](f: A => B): Either[E, B] = this match {
    case l: Left[E] => l
    case Right(a) => Right(f(a))
  }

  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = this match {
    case l: Left[E] => l
    case Right(a) => f(a)
  }

  def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] = this match {
    case Left(e) => b
    case r: Right[A] => r
  }

  def map2[EE >: E, B, C](b: => Either[EE, B])(f: (A, B) => C): Either[EE, C] = this match {
    case l: Left[E] => l
    case Right(a) => b.map(f(a, _))
  }

}

case class Left[+E](get: E) extends Either[E, Nothing]
case class Right[+A](get: A) extends Either[Nothing, A]

object Either {

  def sequence[E, A](es: List[Either[E, A]]): Either[E, List[A]] = {
    traverse(es)(e => e)
  }

  def traverse[E, A, B](as: List[A])(f: A => Either[E, B]): Either[E, List[B]] = {
    as.foldRight[Either[E, List[B]]](Right(Nil))((a, b) => f(a).map2(b)(_ :: _))
  }
}