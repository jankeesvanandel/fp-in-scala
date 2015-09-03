package fp_in_scala.chapter_02

/**
 * Created by jankeesvanandel on 31/08/15.
 */
object HigherOrderFunctions {
  def curry[A, B, C](f: (A, B) => C): A => B => C = {
    a: A => b: B => f(a, b)
  }

  def uncurry[A, B, C](f: A => B => C): (A, B) => C = {
    (a: A, b: B) => f(a)(b)
  }

  def compose[A, B, C](f: B => C, g: A => B): A => C = {
    (a: A) => f(g(a))
  }

}
