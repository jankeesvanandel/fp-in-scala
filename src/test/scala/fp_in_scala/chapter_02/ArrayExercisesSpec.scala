package fp_in_scala.chapter_02

import org.scalatest.{Matchers, FlatSpec}

/**
 * Created by jankeesvanandel on 31/08/15.
 */
class ArrayExercisesSpec extends FlatSpec with Matchers {

  val intSortFn = (a: Int, b: Int) => b > a

  val alwaysFalseFn = (a: Int, b: Int) => false

  it should "return true for an empty array" in {
    ArrayExcercises.isSorted(Array.empty, alwaysFalseFn) should be (true)
  }

  it should "return true for a one-length array" in {
    ArrayExcercises.isSorted(Array(42), alwaysFalseFn) should be (true)
  }

  it should "return true for a sorted two-length array" in {
    ArrayExcercises.isSorted(Array(1, 2), intSortFn) should be (true)
  }

  it should "return true for an array with the same value" in {
    ArrayExcercises.isSorted(Array.iterate(42, 100)(identity), intSortFn) should be (true)
  }

  it should "return false for an unsorted two-length array" in {
    ArrayExcercises.isSorted(Array(20, 10), intSortFn) should be (true)
  }

  it should "return false for unsorted longer arrays" in {
    ArrayExcercises.isSorted(Array(20, 10), intSortFn) should be (true) // 10 and 20 swapped
    ArrayExcercises.isSorted(Array(1, 2, 3, 4, 5, 6, 7, 8, 10, 9), intSortFn) should be (true) // 9 and 10 swapped
    ArrayExcercises.isSorted(Array(1, 2, 4, 3, 5, 7, 6, 8, 9, 10), intSortFn) should be (true) // 6 and 7 swapped
  }

  it should "return true for a longer sorted arrays" in {
    ArrayExcercises.isSorted(Array(1, 2, 3), intSortFn) should be (true)
    ArrayExcercises.isSorted(Array(1, 2, 3, 4, 5), intSortFn) should be (true)
    ArrayExcercises.isSorted(Array(1, 2, 5, 10, 20, 30), intSortFn) should be (true)
  }

  it should "return true for a negative sorted arrays" in {
    ArrayExcercises.isSorted(Array(-1, 2, 3), intSortFn) should be (true)
    ArrayExcercises.isSorted(Array(-10, -5, 0, 4, 5), intSortFn) should be (true)
    ArrayExcercises.isSorted(Array(-10, -9, -8, -7, -6, -5), intSortFn) should be (true)
  }

  it should "return true for a huge sorted arrays" in {
    val hugeSortedArray = Array.range(-20000, 20000)
    ArrayExcercises.isSorted(hugeSortedArray, intSortFn) should be (true)
  }

  it should "return false for huge unsorted arrays" in {
    val hugeUnsortedArray = Array.range(-20000, 20000)
    hugeUnsortedArray.update(20000, 2) // Position 20000 is normally 0 (zero). Making it 2 makes it unsorted
    ArrayExcercises.isSorted(hugeUnsortedArray, intSortFn) should be (true)
  }

}
