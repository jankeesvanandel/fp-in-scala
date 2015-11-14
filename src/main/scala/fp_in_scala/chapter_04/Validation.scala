package fp_in_scala.chapter_04

/**
  * Created by jankeesvanandel on 14/11/15.
  */
sealed trait Validation[+E, +A] {
  def map[B](f: A => B): Validation[E, B] = this match {
    case l: Errors[E] => l
    case Success(a) => Success(f(a))
  }

  def map2[EE >: E, B, C](b: => Validation[EE, B])(f: (A, B) => C): Validation[EE, C] = this match {
    case l1: Errors[E] => b match {
      case l2: Errors[E] => Errors(l1.get ++ l2.get)
      case _ => l1
    }
    case Success(a) => b.map(f(a, _))
  }

}

case class Errors[+E](get: Seq[E]) extends Validation[E, Nothing]
case class Success[+A](get: A) extends Validation[Nothing, A]

object Validation {

  def sequence[E, A](es: List[Validation[E, A]]): Validation[E, List[A]] = {
    traverse(es)(e => e)
  }

  def traverse[E, A, B](as: List[A])(f: A => Validation[E, B]): Validation[E, List[B]] = {
    as.foldRight[Validation[E, List[B]]](Success(Nil))((a, b) => f(a).map2(b)(_ :: _))
  }
}