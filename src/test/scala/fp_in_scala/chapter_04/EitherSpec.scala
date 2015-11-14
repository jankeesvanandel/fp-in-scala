package fp_in_scala.chapter_04

import org.scalatest.prop.PropertyChecks
import org.scalatest.{FreeSpec, Matchers}

class EitherSpec extends FreeSpec with Matchers with PropertyChecks {

  val leftEither = Left("Error").asInstanceOf[Either[String, Int]]
  def intFn(a: Int, b: Int): Int = a + b

  "Either" - {
    "when Right" - {
      "should implement map" in {
        Right(5).map(_ + 1) should be(Right(6))
      }

      "should implement flatMap" in {
        Right(5).flatMap(s => Right(s + 1)) should be(Right(6))
      }

      "should implement orElse" in {
        Right(5).orElse(Right(1)) should be(Right(5))
      }

      "should implement map2" in {
        Right(5).map2(leftEither)(intFn) should be(leftEither)
        Right(5).map2(Right(1))(intFn) should be(Right(6))
      }
    }

    "when Left" - {
      "should implement map" in {
        leftEither.map(_ + 1) should be(leftEither)
      }

      "should implement flatMap" in {
        leftEither.flatMap(s => Right(s + 1)) should be(leftEither)
      }

      "should implement orElse" in {
        leftEither.orElse(Right(1)) should be(Right(1))
      }

      "should implement map2" in {
        leftEither.map2(leftEither)(intFn) should be(leftEither)
        leftEither.map2(Right(1))(intFn) should be(leftEither)
      }
    }

    "companion object" - {
      "should implement sequence" in {
        Either.sequence(List.empty) should be(Right(List.empty))
        Either.sequence(List(Right(1), Left(""), Right(3))) should be(Left(""))
        Either.sequence(List(Right(1), Right(2), Right(3))) should be(Right(List(1, 2, 3)))
      }

      "should implement traverse" in {
        def fn(i: Int): Either[String, Int] =
          if (i >= 0) Right(i)
          else Left("")

        Either.traverse(List.empty)(fn) should be(Right(List.empty))
        Either.traverse(List(-1))(fn) should be(Left(""))
        Either.traverse(List(1, 2, 3))(fn) should be(Right(List(1, 2, 3)))
        Either.traverse(List(1, 2, -3))(fn) should be(Left(""))
      }
    }
  }
}
