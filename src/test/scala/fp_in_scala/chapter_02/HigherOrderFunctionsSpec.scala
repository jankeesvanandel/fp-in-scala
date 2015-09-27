package fp_in_scala.chapter_02

import org.scalatest.{Matchers, FlatSpec}

class HigherOrderFunctionsSpec extends FlatSpec with Matchers {

  // Types for making the intent of the function types clearer
  type Curried = Int => Long => Double
  type Uncurried = (Int, Long) => Double

  it should "give a curried version of the function back" in {
    val fn: Uncurried = (i: Int, l: Long) => i + l + 0.5

    val curried: Curried = HigherOrderFunctions.curry(fn)

    curried(1)(2L) should be (3.5)
  }

  it should "give an uncurried version of the function back" in {
    val fn: Curried = (i: Int) => (l: Long) => i + l + 0.5

    val uncurried: Uncurried = HigherOrderFunctions.uncurry(fn)

    uncurried(1, 2L) should be (3.5)
  }

  it should "compose 2 functions" in {
    val fn1 = (i: Int) => i * 2
    val fn2 = (i: Int) => i + 3

    val composed = HigherOrderFunctions.compose(fn2, fn1)

    // ((1 * 2) + 3) = 5, NOT ((1 + 3) * 5) = 20
    composed(1) should be (5)
  }

}
