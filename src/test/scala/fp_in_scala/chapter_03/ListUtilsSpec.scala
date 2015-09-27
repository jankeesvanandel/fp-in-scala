package fp_in_scala.chapter_03

import fp_in_scala.chapter_03.ListUtils.fromScala
import org.scalatest.prop.PropertyChecks
import org.scalatest.{FlatSpec, Matchers}

class ListUtilsSpec extends FlatSpec with PropertyChecks with Matchers {

  private val listFromOneToTen = List.range(1, 11)
  private val listFromElevenToTwenty = List.range(11, 21)

  it should "convert from Scala collections" in {
    fromScala(scala.List.empty) should be(Nil)
    fromScala(scala.List(1, 2, 3)) should be(Cons(1, Cons(2, Cons(3, Nil))))
  }

  it should "convert to Scala collections" in {
    ListUtils.toScala(Nil) should be(scala.List.empty)
    ListUtils.toScala(Cons(1, Cons(2, Cons(3, Nil)))) should be(scala.List(1, 2, 3))
  }

  it should "return an error if head is called on an empty list" in {
    an[java.lang.RuntimeException] should be thrownBy {
      Nil.head
    }
  }

  it should "return an error if tail is called on an empty list" in {
    an[java.lang.RuntimeException] should be thrownBy {
      Nil.tail
    }
  }

  it should "return Nil if tail is called on a one-length-list" in {
    List(1).tail should be(Nil)
  }

  it should "return the tail if tail is called on longer list" in {
    List(1, 2).tail should be(List(2))
    List(1, 2, 3).tail should be(List(2, 3))
    List(1, 2, 3, 4).tail should be(List(2, 3, 4))
    List(1, 2, 3, 4, 5).tail should be(List(2, 3, 4, 5))
  }

  it should "return the tail if tail is called on a huge list" in {
    val hugeList = List(Array.range(1, 1000000))
    Cons(0, hugeList).tail should be(hugeList)
  }

  it should "return Nil if setHead is called on an empty list" in {
    ListUtils.setHead(Nil)(1) should be(Nil)
  }

  it should "return a new list with the head replaced" in {
    ListUtils.setHead(List(1, 2))(3) should be(List(3, 2))
  }

  it should "return Nil if drop is called on an empty list" in {
    Nil.drop(1) should be(Nil)
  }

  it should "return Nil if drop is called on a list which is exactly as big as 'n'" in {
    List(1).drop(1) should be(Nil)
    List(1, 2).drop(2) should be(Nil)
    List(1, 2, 3, 4, 5).drop(5) should be(Nil)
  }

  it should "return a list with the last element if drop is called on a list which is only one longer than 'n'" in {
    List(1).drop(0) should be(Cons(1, Nil))
    List(1, 2).drop(1) should be(Cons(2, Nil))
    List(1, 2, 3, 4, 5).drop(4) should be(Cons(5, Nil))
  }

  it should "return the tail of a list if drop is called" in {
    forAll { (l: scala.List[Int], n: Int) =>
      whenever(condition = true) {
        fromScala(l).drop(n) should be(fromScala(l.drop(n)))
      }
    }
  }

  it should "return Nil if dropWhile is called on an empty list" in {
    Nil.dropWhile((i: Int) => false) should be(Nil)
  }

  it should "return the correct list if dropWhile is called on a list" in {
    def lowerThan(n: Int)(i: Int) = i < n
    List(1).dropWhile(lowerThan(0)) should be(List(1))
    List(1).dropWhile(lowerThan(1)) should be(List(1))
    List(1, 2).dropWhile(lowerThan(2)) should be(List(2))
    List(1, 2, 3, 4, 5).dropWhile(lowerThan(3)) should be(List(3, 4, 5))
    List(1, 2, 3, 4, 5).dropWhile(lowerThan(10)) should be(Nil)

    List(1, 2, 3).dropWhile((x: Int) => {
      x % 2 == 0
    }) should be(List(1, 2, 3))
  }

  it should "return the whole list except the last if init is called on a list" in {
    Nil.init should be(Nil)
    List(1).init should be(Nil)
    List(1, 2, 3).init should be(List(1, 2))
    List(1, 2, 3, 4, 5).init should be(List(1, 2, 3, 4))
  }

  it should "work like a proper foldRight blah blah" in {
    val result = List(1, 2, 3).foldRight(Nil: List[Int])(Cons(_, _))
    result should be(List(1, 2, 3))
  }

  it should "return the length using foldRight" in {
    List.range(0, 3).lengthUsingFoldRight should be(3)
    List.range(0, 10).lengthUsingFoldRight should be(10)
    List.range(0, 100).lengthUsingFoldRight should be(100)

    an[java.lang.StackOverflowError] should be thrownBy {
      List.range(0, 100000).lengthUsingFoldRight
    }
  }

  it should "work like a proper foldLeft blah blah" in {
    List.range(1, 4).foldLeft(0)(_ + _) should be(6)
    List.range(1, 4).foldLeft(Nil: List[Int])((b, a) => Cons(a, b)) should be(List.range(3, 0, -1))
  }

  it should "work like a tail-recursive foldLeft" in {
    val result = List.range(0, 100000).foldLeft(0)((b, a) => b + 1)
    result should be(100000)
  }

  it should "calc the sum using foldLeft" in {
    val result = ListUtils.sumUsingFoldLeft(listFromOneToTen)
    result should be(55)
  }

  it should "calc the product using foldLeft" in {
    val result = ListUtils.productUsingFoldLeft(listFromOneToTen)
    result should be(3628800)
  }

  it should "calc the length using foldLeft" in {
    val result = ListUtils.lengthUsingFoldLeft(listFromOneToTen)
    result should be(10)
  }

  it should "calc the reverse using foldLeft" in {
    val result = ListUtils.reverseUsingFoldLeft(List.range(1, 4))
    result should be(List(3, 2, 1))
  }

  it should "implement foldRight in terms of foldLeft" in {
    ListUtils.foldRightUsingFoldLeft(listFromOneToTen, 0)(_ + _) should be(55)

    val result = ListUtils.foldRightUsingFoldLeft(List(1, 2, 3), Nil: List[Int])(Cons(_, _))
    result should be(List(1, 2, 3))
  }

  it should "implement foldLeft in terms of foldRight" in {
    ListUtils.foldLeftUsingFoldRight(listFromOneToTen, 0)(_ + _) should be(55)

    val result = ListUtils.foldLeftUsingFoldRight(List(1, 2, 3), Nil: List[Int])((b, a) => Cons(a, b))
    result should be(List(1, 2, 3))
  }

  it should "implement append using foldLeft" in {
    ListUtils.appendUsingFoldLeft(listFromOneToTen, listFromElevenToTwenty) should be(List.range(1, 21))
    ListUtils.appendUsingFoldLeft(listFromOneToTen, Nil) should be(listFromOneToTen)
    ListUtils.appendUsingFoldLeft(Nil, listFromOneToTen) should be(listFromOneToTen)
  }

  it should "implement append using foldRight" in {
    ListUtils.appendUsingFoldRight(listFromOneToTen, listFromElevenToTwenty) should be(List.range(1, 21))
    ListUtils.appendUsingFoldRight(listFromOneToTen, Nil) should be(listFromOneToTen)
    ListUtils.appendUsingFoldRight(Nil, listFromOneToTen) should be(listFromOneToTen)
  }

  it should "implement concatenateListOfLists" in {
    val result = ListUtils.concatenateListOfLists(List(
      List(1, 2, 3),
      List(4, 5, 6),
      List(7, 8, 9)
    ))
    result should be(List.range(1, 10))
  }

  it should "map a list of ints by increasing with one and turning it into a list of strings" in {
    val result = List.range(0, 100).map(a => (a + 1).toString)
    result should be(List.range(1, 101).map(_.toString))
  }

  it should "filter non-matching elements from a list" in {
    List.range(0, 100).filter(_ => false) should be(Nil)
    List.range(0, 100).filter(_ => true) should be(List.range(0, 100))
    List.range(0, 100).filter(i => i < 50) should be(List.range(0, 50))
    List.range(0, 100).filter(i => i >= 50) should be(List.range(50, 100))
    List.range(0, 100).filter(i => i % 2 == 0) should be(List.range(0, 100, 2))
  }

  it should "flatMap a List[Int]s into a List of List[List[Int]]s" in {
    val result = List.range(1, 4).flatMap(a => List(a, a))
    result should be(List(1, 1, 2, 2, 3, 3))
  }

  it should "filter non-matching elements from a list using flatMap" in {
    List.range(0, 100).filterUsingFlatMap(_ => false) should be(Nil)
    List.range(0, 100).filterUsingFlatMap(_ => true) should be(List.range(0, 100))
    List.range(0, 100).filterUsingFlatMap(i => i < 50) should be(List.range(0, 50))
    List.range(0, 100).filterUsingFlatMap(i => i >= 50) should be(List.range(50, 100))
    List.range(0, 100).filterUsingFlatMap(i => i % 2 == 0) should be(List.range(0, 100, 2))
  }

  it should "add two int lists" in {
    ListUtils.addTwoIntLists(Nil, Nil) should be(Nil)
    ListUtils.addTwoIntLists(List(1, 2, 3), List(4, 5, 6)) should be(List(5, 7, 9))
    an[RuntimeException] should be thrownBy {
      ListUtils.addTwoIntLists(Nil, List(4, 5, 6))
    }
    an[RuntimeException] should be thrownBy {
      ListUtils.addTwoIntLists(List(1, 2, 3), Nil)
    }
  }

  it should "zip two lists into another list" in {
    val noOpCombiningFn = (_: Int, _: Int) => 1
    Nil.zipWith(Nil)(noOpCombiningFn) should be(Nil)
    List(1, 2, 3).zipWith(List(".1", ".2", ".3"))((i, s) => (i + s).toDouble) should be(List[Double](1.1, 2.2, 3.3))
    an[RuntimeException] should be thrownBy {
      Nil.zipWith(List(4, 5, 6))(noOpCombiningFn)
    }
    an[RuntimeException] should be thrownBy {
      List(1, 2, 3).zipWith(Nil)(noOpCombiningFn)
    }
  }

  it should "toString properly" in {
    Nil.toString should be("List()")
    List(1, 2, 3).toString should be("List(1,2,3)")
    List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10).toString should be("List(1,2,3,4,5,6,7,8,9,10)")
  }

  it should "hasSubSequence should work properly for Nil cases" in {
    ListUtils.hasSubsequence(Nil: List[Int], Nil: List[Int]) should be(true)
    ListUtils.hasSubsequence(List(1, 2, 3), Nil) should be(true)
  }

  it should "hasSubSequence should work properly for simple cases" in {
    ListUtils.hasSubsequence(List(1, 2, 3), List(1)) should be(true)
    ListUtils.hasSubsequence(List(1, 2, 3), List(2)) should be(true)
    ListUtils.hasSubsequence(List(1, 2, 3), List(3)) should be(true)
    ListUtils.hasSubsequence(List(1, 2, 3), List(1, 2)) should be(true)
    ListUtils.hasSubsequence(List(1, 2, 3), List(2, 3)) should be(true)
    ListUtils.hasSubsequence(List(1, 2, 3), List(1, 2, 3)) should be(true)
  }

  it should "hasSubSequence should work properly with gaps" in {
    ListUtils.hasSubsequence(List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10), List(5, 7, 9)) should be(false)
    ListUtils.hasSubsequence(List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10), List(1, 10)) should be(false)
  }

  it should "hasSubSequence should retry when the subsequence is not completely found" in {
    ListUtils.hasSubsequence(List(1, 2, 3, 4, 5, 6, 2, 3, 9, 10), List(2, 3, 9)) should be(true)
  }
}
