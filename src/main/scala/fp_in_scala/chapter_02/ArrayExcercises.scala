package fp_in_scala.chapter_02

import scala.annotation.tailrec

/**
 * Created by jankeesvanandel on 31/08/15.
 */
object ArrayExcercises {

  def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {
    @tailrec
    def doIsSorted(as: Array[A], index: Int, acc: Boolean): Boolean = {
      if (as.length <= index) true
      else {
        val newAcc = acc && ordered(as(index-1), as(index))
        doIsSorted(as, index + 1, newAcc)
      }
    }
    if (as.isEmpty || as.length == 1) true
    else doIsSorted(as, 1, acc = true)
  }
}
