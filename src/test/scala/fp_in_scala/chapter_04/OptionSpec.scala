package fp_in_scala.chapter_04

import org.scalatest.prop.PropertyChecks
import org.scalatest.{FreeSpec, Matchers}

class OptionSpec extends FreeSpec with Matchers with PropertyChecks {

  "Option" - {
    "when Some" - {
      "should implement map" in {
        Some("hello").map(_.length) should be(Some(5))
      }

      "should implement flatMap" in {
        Some("hello").flatMap(s => Some(s.length)) should be(Some(5))
      }

      "should implement filter" in {
        Some("hello").filter(_.length == 5) should be(Some("hello"))
        Some("hello").filter(_.length == 4) should be(None)
      }

      "should implement getOrElse" in {
        Some("hello").getOrElse("Nooo") should be("hello")
      }

      "should implement orElse" in {
        Some("hello").orElse(Some("Nooo")) should be(Some("hello"))
      }
    }

    "when None" - {
      "should implement map" in {
        None.asInstanceOf[Option[String]].map(_.length) should be(None)
      }

      "should implement flatMap" in {
        None.asInstanceOf[Option[String]].flatMap(s => Some(s.length)) should be(None)
      }

      "should implement filter" in {
        None.asInstanceOf[Option[String]].filter(_.length == 5) should be(None)
      }

      "should implement getOrElse" in {
        None.asInstanceOf[Option[String]].getOrElse("Nooo") should be("Nooo")
      }

      "should implement orElse" in {
        None.asInstanceOf[Option[String]].orElse(Some("Nooo")) should be(Some("Nooo"))
      }
    }

    "companion object" - {
      "should implement mean" in {
        Option.mean(Seq(1, 2, 3, 4, 5)) should be(Some(3))
        Option.mean(Seq()) should be(None)
      }

      "should implement variance" in {
        Option.variance(Nil) should be(None)

        forAll { (xs: Seq[Double]) =>
          whenever(xs.nonEmpty) {
            Option.variance(xs) should be(Some(testAgainstVariance(xs)))
          }
        }

        // Pulled from the internet to test against
        def testAgainstVariance(items: Seq[Double]): Double = {
          def mean(items: Seq[Double]) = {
            items.sum / items.size
          }
          val itemMean = mean(items)
          val count = items.size
          val sumOfSquares = items.foldLeft(0.0d)((total, item) => {
            val itemDbl = item
            val square = math.pow(itemDbl - itemMean, 2)
            total + square
          })
          sumOfSquares / count.toDouble
        }
      }

      "should implement map2" in {
        def fn(a: Int, b: Int): Int = a + b

        Option.map2(None, None)(fn) should be(None)
        Option.map2(None, Some(1))(fn) should be(None)
        Option.map2(Some(1), None)(fn) should be(None)
        Option.map2(Some(1), Some(2))(fn) should be(Some(3))
      }

      "should implement sequence" in {
        Option.sequence(List.empty) should be(Some(List.empty))
        Option.sequence(List(Some(1), None, Some(3))) should be(None)
        Option.sequence(List(Some(1), Some(2), Some(3))) should be(Some(List(1, 2, 3)))
      }

      "should implement traverse" in {
        def fn(i: Int): Option[Int] =
          if (i >= 0) Some(i)
          else None

        Option.traverse(List.empty)(fn) should be(Some(List.empty))
        Option.traverse(List(-1))(fn) should be(None)
        Option.traverse(List(1, 2, 3))(fn) should be(Some(List(1, 2, 3)))
        Option.traverse(List(1, 2, -3))(fn) should be(None)
      }

      "should implement sequenceInTermsOfTraverse" in {
        Option.sequenceInTermsOfTraverse(List.empty) should be(Some(List.empty))
        Option.sequenceInTermsOfTraverse(List(Some(1), None, Some(3))) should be(None)
        Option.sequenceInTermsOfTraverse(List(Some(1), Some(2), Some(3))) should be(Some(List(1, 2, 3)))
      }
    }
  }
}
