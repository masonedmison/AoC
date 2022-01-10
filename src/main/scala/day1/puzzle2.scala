package day1

import inputs._
import scala.collection.mutable.ArrayBuffer

// same input as puzzle2

object puzzle2 extends App {
  val p2In = puzzle1.split("\\n").filter(_.nonEmpty).map(_.trim().toInt).toList

  def slidingThree(ints: List[Int]): Int = {
    var i                           = 0
    var currThree: ArrayBuffer[Int] = ArrayBuffer.empty[Int]
    var prevSum                     = -1
    var accInc                      = 0
    while (i < ints.length || currThree.length == 3) {
      if (currThree.length == 3) {
        accInc = if (currThree.sum > prevSum && prevSum != -1) accInc + 1 else accInc
        prevSum = currThree.sum
        currThree = ArrayBuffer.empty[Int]
        i -= 2
      } else {
        currThree.addOne(ints(i))
        i += 1
      }
    }
    accInc
  }
  println(s"Length of ${p2In.length}")
  println(s"solution: ${slidingThree(p2In)}")

}
