package fp_in_scala.chapter_03

import org.scalatest.prop.PropertyChecks
import org.scalatest.{FlatSpec, Matchers}

/**
 * Created by jankeesvanandel on 31/08/15.
 */
class ListUtilsSpec extends FlatSpec with PropertyChecks with Matchers {

  it should "return an error if tail is called on an empty list" in {
    ListUtils.tail(Nil) should be(Left("Tail of an empty list!"))
  }

  it should "return Nil if tail is called on a one-length-list" in {
    ListUtils.tail(List(1)) should be(Right(Nil))
  }

  it should "return the tail if tail is called on longer list" in {
    ListUtils.tail(List(1, 2)) should be(Right(List(2)))
    ListUtils.tail(List(1, 2, 3)) should be(Right(List(2, 3)))
    ListUtils.tail(List(1, 2, 3, 4)) should be(Right(List(2, 3, 4)))
    ListUtils.tail(List(1, 2, 3, 4, 5)) should be(Right(List(2, 3, 4, 5)))
  }

  it should "return the tail if tail is called on a huge list" in {
    val hugeList = List(Array.range(1, 1000000))
    ListUtils.tail(0 :: hugeList) should be(Right(hugeList))
  }

  it should "return an error if setHead is called on an empty list" in {
    ListUtils.setHead(Nil, 1) should be(Left("Head of an empty list!"))
  }

  it should "return a new list with the head replaced" in {
    ListUtils.setHead(List(1, 2), 3) should be(Right(List(3, 2)))
  }

  it should "return Nil if drop is called on an empty list" in {
    ListUtils.drop(Nil, 1) should be(Nil)
  }

  it should "return Nil if drop is called on a list which is exactly as big as 'n'" in {
    ListUtils.drop(List(1), 1) should be(Nil)
    ListUtils.drop(List(1, 2), 2) should be(Nil)
    ListUtils.drop(List(1, 2, 3, 4, 5), 5) should be(Nil)
  }

  it should "return a list with the last element if drop is called on a list which is only one longer than 'n'" in {
    ListUtils.drop(List(1), 0) should be(List(1))
    ListUtils.drop(List(1, 2), 1) should be(List(2))
    ListUtils.drop(List(1, 2, 3, 4, 5), 4) should be(List(5))
  }

  it should "return the tail of a list if drop is called" in {
    forAll { (l: List[Int], n: Int) =>
      whenever(condition = true) {
        ListUtils.drop(l, n) should be(l.drop(n))
      }
    }
  }

  it should "return Nil if dropWhile is called on an empty list" in {
    ListUtils.dropWhile(Nil, (i: Int) => false) should be(Nil)
  }

  it should "return the correct list if dropWhile is called on a list" in {
    def lowerThan(n: Int)(i: Int) = i < n
    ListUtils.dropWhile(List(1), lowerThan(0)) should be(List(1))
    ListUtils.dropWhile(List(1), lowerThan(1)) should be(List(1))
    ListUtils.dropWhile(List(1, 2), lowerThan(2)) should be(List(2))
    ListUtils.dropWhile(List(1, 2, 3, 4, 5), lowerThan(3)) should be(List(3, 4, 5))
    ListUtils.dropWhile(List(1, 2, 3, 4, 5), lowerThan(10)) should be(Nil)
  }

  it should "return the whole list except the last if init is called on a list" in {
    ListUtils.init(Nil) should be(Nil)
    ListUtils.init(List(1)) should be(Nil)
    ListUtils.init(List(1, 2, 3)) should be(List(1, 2))
    ListUtils.init(List(1, 2, 3, 4, 5)) should be(List(1, 2, 3, 4))
  }

  it should "work like a proper foldRight blah blah" in {
    val result = ListUtils.foldRight(List(1, 2, 3), Nil: List[Int])(_ :: _)
    result should be(List(1, 2, 3))
  }

  it should "return the length using foldRight" in {
    ListUtils.lengthUsingFoldRight(List.range(0, 3)) should be(3)
    ListUtils.lengthUsingFoldRight(List.range(0, 10)) should be(10)
    ListUtils.lengthUsingFoldRight(List.range(0, 100)) should be(100)

    an [java.lang.StackOverflowError] should be thrownBy {
      ListUtils.lengthUsingFoldRight(List.range(0, 100000))
    }
  }

  it should "work like a proper foldLeft blah blah" in {
    val result = ListUtils.foldLeft(List.range(1, 4), 0: Int)(_ + _)
    result should be(6)
  }

  it should "work like a tail-recursive foldLeft" in {
    val result = ListUtils.foldLeft(List.range(0, 100000), 0: Int)((b, a) => b+1)
    result should be(100000)
  }
}
