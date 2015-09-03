package fp_in_scala.chapter_02

import scala.annotation.tailrec

object Fibonacci {

  private def invalidNError(n: Int): Left[String, Nothing] = {
    Left(s"value $n is lower than 0 and not allowed for fibonacci")
  }

  def fib(n: Int): Either[String, Int] = {
    if (n < 0) invalidNError(n)
    else {
      @tailrec
      def doFib(acc: Int, current: Int, counter: Int): Int =
        if (counter == n) acc
        else doFib(current, acc + current, counter + 1)
      Right(doFib(1, 0, 0))
    }
  }

  def fibBig(n: Int): Either[String, BigInt] = {
    if (n < 0) invalidNError(n)
    else {
      @tailrec
      def doFib(acc: BigInt, current: BigInt, counter: Int): BigInt =
        if (counter == n) acc
        else doFib(current, acc + current, counter + 1)
      Right(doFib(1, 0, 0))
    }
  }
}