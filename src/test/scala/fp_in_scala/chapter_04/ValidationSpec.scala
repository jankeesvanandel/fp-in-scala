package fp_in_scala.chapter_04

import org.scalatest.prop.PropertyChecks
import org.scalatest.{FreeSpec, Matchers}

class ValidationSpec extends FreeSpec with Matchers with PropertyChecks {

  val errors1Validation = Errors(Seq("Error 1")).asInstanceOf[Validation[String, Int]]
  val errors2Validation = Errors(Seq("Error 2")).asInstanceOf[Validation[String, Int]]
  def intFn(a: Int, b: Int): Int = a + b

  "Validation" - {
    "when Success" - {
      "should implement map" in {
        Success(5).map(_ + 1) should be(Success(6))
      }

      "should implement map2" in {
        Success(5).map2(errors1Validation)(intFn) should be(errors1Validation)
        Success(5).map2(Success(1))(intFn) should be(Success(6))
      }
    }

    "when Errors" - {
      "should implement map" in {
        errors1Validation.map(_ + 1) should be(errors1Validation)
      }

      "should implement map2" in {
        errors1Validation.map2(errors2Validation)(intFn).asInstanceOf[Errors[String]].get should be(Seq("Error 1", "Error 2"))
        errors1Validation.map2(Success(1))(intFn) should be(errors1Validation)
      }
    }

    "companion object" - {
      "should implement sequence" in {
        Validation.sequence(List.empty) should be(Success(List.empty))
        Validation.sequence(List(Success(1), Errors(""), Success(3))) should be(Errors(""))
        Validation.sequence(List(Success(1), Success(2), Success(3))) should be(Success(List(1, 2, 3)))
      }

      "should implement traverse" in {
        def fn(i: Int): Validation[String, Int] =
          if (i >= 0) Success(i)
          else Errors(Seq(s"Error $i"))

        Validation.traverse(List.empty)(fn) should be(Success(List.empty))
        Validation.traverse(List(-1))(fn) should be(Errors(Seq("Error -1")))
        Validation.traverse(List(-1, -2))(fn) should be(Errors(Seq("Error -1", "Error -2")))
        Validation.traverse(List(-1, -2, -3))(fn) should be(Errors(Seq("Error -1", "Error -2", "Error -3")))
        Validation.traverse(List(1, 2, 3))(fn) should be(Success(List(1, 2, 3)))
        Validation.traverse(List(1, 2, -3))(fn) should be(Errors(Seq("Error -3")))
      }
    }
  }
}
