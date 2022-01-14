package day3

import inputs._

object puzzle2 extends App {
  val p1In = inputs.p1.split("\\n").filter(_.nonEmpty).map(_.trim).filter(_.nonEmpty).toVector

  def rating(vec: Vector[String], ratingF: (Int, Int) => Int): Int = {
    def go(
        toVis: List[Int],
        zeros: List[Int],
        ones: List[Int],
        curBitPos: Int
    ): Int = {
      if (toVis.isEmpty && zeros.length + ones.length == 1) {
        val remainingI = zeros.headOption.orElse(ones.headOption).get
        Integer.parseInt(vec(remainingI), 2)
      } else if (toVis.isEmpty) {
        val bitToK = ratingF(ones.length, zeros.length)
        val cbN    = if (curBitPos >= 11) 0 else curBitPos + 1
        if (bitToK == 1) go(ones, List.empty, List.empty, cbN)
        else go(zeros, List.empty, List.empty, cbN)
      } else {
        val h    = toVis.head
        val curI = vec(h)(curBitPos).toString.toInt
        if (curI == 0) go(toVis.tail, h :: zeros, ones, curBitPos)
        else go(toVis.tail, zeros, h :: ones, curBitPos)
      }
    }
    go((0 to vec.length - 1).toList, List.empty, List.empty, 0)
  }

  println(s"Length of input: ${p1In.length}")

  val oxR  = rating(p1In, (i1, i2) => if (i1 >= i2) 1 else 0)
  val co2R = rating(p1In, (i1, i2) => if (i1 >= i2) 0 else 1)

  // correct answer should be 2784375 (sanity check for refactor)
  println(s"result: ${oxR * co2R}")

}
