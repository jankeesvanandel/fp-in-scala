package fp_in_scala.chapter_05

import org.scalatest.prop.PropertyChecks
import org.scalatest.{FreeSpec, Matchers}
import fp_in_scala.chapter_03._
import fp_in_scala.chapter_04._

class StreamSpec extends FreeSpec with Matchers with PropertyChecks {

  "Stream" - {
    "trait" - {
      "should implement headOption" in {
        EmptyStream.headOption should be(None)
        Stream.apply(1, 2, 3).headOption should be(Some(1))
      }

      "should implement toList" in {
        EmptyStream.toList should be(Nil)
        Stream(1, 2, 3).toList should be(List(1, 2, 3))
      }

      "should implement take(int)" in {
        Stream(1, 2, 3).take(-1) should be(EmptyStream)
        Stream(1, 2, 3).take(0) should be(EmptyStream)
        Stream(1, 2, 3).take(1) should be(Stream(1))
        Stream(1, 2, 3).take(3) should be(Stream(1, 2, 3))
        Stream(1, 2, 3).take(5) should be(Stream(1, 2, 3))
        Stream(1, 2, 3).take(Int.MaxValue) should be(Stream(1, 2, 3))
      }

      "should implement takeWhile(boolean)" in {
        Stream(1, 2, 3).takeWhile(_ => false) should be(EmptyStream)
        Stream(1, 2, 3).takeWhile(i => i <= 1) should be(Stream(1))
        Stream(1, 2, 3).takeWhile(i => i <= 3) should be(Stream(1, 2, 3))
        Stream(1, 2, 3).takeWhile(_ => true) should be(Stream(1, 2, 3))
      }

      "should implement drop(int)" in {
        Stream(1, 2, 3).drop(-1) should be(Stream(1, 2, 3))
        Stream(1, 2, 3).drop(0) should be(Stream(1, 2, 3))
        Stream(1, 2, 3).drop(1) should be(Stream(2, 3))
        Stream(1, 2, 3).drop(3) should be(EmptyStream)
        Stream(1, 2, 3).drop(5) should be(EmptyStream)
        Stream(1, 2, 3).drop(Int.MaxValue) should be(EmptyStream)
      }

      "should implement dropWhile(boolean)" in {
        Stream(1, 2, 3).dropWhile(_ => false) should be(Stream(1, 2, 3))
        Stream(1, 2, 3).dropWhile(i => i <= 1) should be(Stream(2, 3))
        Stream(1, 2, 3).dropWhile(i => i <= 3) should be(EmptyStream)
        Stream(1, 2, 3).dropWhile(_ => true) should be(EmptyStream)
      }

      "should implement forAll(boolean)" in {
        Stream(1, 2, 3).forAll(_ => false) should be(false)
        Stream(1, 2, 3).forAll(i => i <= 1) should be(false)
        Stream(1, 2, 3).forAll(i => i <= 3) should be(true)
        Stream(1, 2, 3).forAll(_ => true) should be(true)
      }

      "should implement append(Stream)" in {
        Stream(1, 2, 3).append(Stream(4, 5, 6)) should be(Stream(1, 2, 3, 4, 5, 6))
      }

      "should implement map(A => B)" in {
        Stream(1, 2, 3).map(_ * 10) should be(Stream(10, 20, 30))
      }

      "should implement flatMap(A => Stream[B])" in {
        Stream(1, 2, 3).flatMap(i => Stream.apply(i + 10, i + 20)) should be(Stream(11, 21, 12, 22, 13, 23))
        Stream(1, 2, 3).flatMap(i => Stream.empty) should be(Stream.empty)
      }

      "should implement filter" in {
        Stream(1, 2, 3).filter(_ == 1) should be(Stream(1))
        Stream(1, 2, 3).filter(_ == 2) should be(Stream(2))
        Stream(1, 2, 3).filter(_ == 3) should be(Stream(3))
        Stream(1, 2, 3).filter(_ => true) should be(Stream(1, 2, 3))
        Stream(1, 2, 3).filter(_ => false) should be(Stream.empty)
      }

      "should implement zipWith" in {
        Stream(1, 2, 3).zipWith(Stream(4, 5, 6))(_ + _) should be(Stream(5, 7, 9))
        Stream(1, 2, 3, 4).zipWith(Stream(4, 5, 6))(_ + _) should be(Stream(5, 7, 9))
        Stream(1, 2, 3).zipWith(Stream(4, 5, 6, 7))(_ + _) should be(Stream(5, 7, 9))
        Stream(1, 2, 3).zipWith(Stream.empty[Int])(_ + _) should be(Stream.empty[Int])
        Stream.empty[Int].zipWith(Stream(4, 5, 6))(_ + _) should be(Stream.empty[Int])
      }

      "should implement zipAll" in {
        Stream(1, 2, 3).zipAll(Stream(4, 5, 6)) should be(Stream((Some(1), Some(4)), (Some(2), Some(5)), (Some(3), Some(6))))
        Stream(1, 2, 3).zipAll(Stream(4)) should be(Stream((Some(1), Some(4)), (Some(2), None), (Some(3), None)))
        Stream(1).zipAll(Stream(4, 5, 6)) should be(Stream((Some(1), Some(4)), (None, Some(5)), (None, Some(6))))
        Stream.empty.zipAll(Stream.empty) should be(Stream.empty)
      }

      "should implement startsWith" in {
        Stream(1, 2, 3).startsWith(Stream(1, 2, 3, 4)) should be(false)
        Stream.empty.startsWith(Stream(1)) should be(false)
        Stream(1).startsWith(Stream.empty) should be(true)
        Stream(1).startsWith(Stream(1)) should be(true)
        Stream(1, 2).startsWith(Stream(1)) should be(true)
        Stream(1, 2, 3, 4, 5).startsWith(Stream(1, 2, 3, 4)) should be(true)
        Stream(1, 2, 3, 4, 5).startsWith(Stream(1, 2, 3, 4, 5)) should be(true)
      }

      "should implement tails" in {
        Stream.empty.tails should be(Stream())
        Stream(1).tails should be(Stream(Stream(1)))
        Stream(1, 2).tails should be(Stream(Stream(1, 2), Stream(2)))
        Stream(1, 2, 3).tails should be(Stream(Stream(1, 2, 3), Stream(2, 3), Stream(3)))
      }

      "should implement hasSubsequence" in {
        Stream.empty.hasSubsequence(Stream.empty) should be(false)
        Stream.empty.hasSubsequence(Stream(1)) should be(false)
        Stream(1).hasSubsequence(Stream.empty) should be(true)
        Stream(1).hasSubsequence(Stream(1)) should be(true)
        Stream(1, 2).hasSubsequence(Stream(1)) should be(true)
        Stream(1).hasSubsequence(Stream(1, 2)) should be(false)
        Stream(1, 2, 3, 4, 5, 6).hasSubsequence(Stream(1, 2, 3, 4)) should be(true)
        Stream(1, 2, 3, 4, 5, 6).hasSubsequence(Stream(2, 3, 4)) should be(true)
        Stream(1, 2, 3, 4, 5, 6).hasSubsequence(Stream(3, 4, 5, 6)) should be(true)
      }

      "should implement scanRight" in {
        Stream.empty[Int].scanRight(0)(_ + _) should be(Stream(0))
        Stream(1).scanRight(0)(_ + _) should be(Stream(1, 0))
        Stream(1, 2, 3).scanRight(0)(_ + _) should be(Stream(6, 5, 3, 0))
      }
    }

    "companion object" - {
      "should implement apply" in {
        Stream.apply(1, 2, 3) should be(ConsStream(() => 1, () => ConsStream(() => 2, () => ConsStream(() => 3, () => EmptyStream))))
      }

      "should implement constant(A)" in {
        Stream.constant(1).take(5) should be(Stream(1, 1, 1, 1, 1))
      }

      "should implement from(Int)" in {
        Stream.from(5).take(5) should be(Stream(5, 6, 7, 8, 9))
      }

      "should implement fibs" in {
        Stream.fibs.take(10) should be(Stream(0, 1, 1, 2, 3, 5, 8, 13, 21, 34))
      }

      "should implement unfold" in {
        Stream.unfold("")(s => Some((s + "1", s + "1"))).take(3) should be(Stream("1", "11", "111"))
        Stream.unfold("")(s => {
          if (s.length < 5) Some((s + "1", s + "1"))
          else None
        }).take(10) should be(Stream("1", "11", "111", "1111", "11111"))
        Stream.unfold("")(_ => None) should be(Stream.empty)
      }
    }
  }
}
