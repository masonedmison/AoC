package day3

import scala.collection.mutable.ArrayBuffer

object puzzle1 extends App {
  val p1In = inputs.p1.split("\\n").filter(_.nonEmpty).map(_.trim).toVector

  println(s"Length of inputs: ${p1In.length}")

  def powerCalc(input: Vector[String]): Int = {
    val bitL     = input(0).length()
    val inpL     = input.length
    val bitCount = ArrayBuffer.fill(bitL)(0)

    var i = 0

    while (i < input.length) {
      input(i).zip(0 to bitL).foreach {
        case (c, i) => bitCount(i) += c.toString.toInt
      }
      i += 1
    }

    println(s"intermediate bitCount ${bitCount}")
    val (gamma, epsilon) = bitCount.foldRight(("", "")) {
      case (bC, (gS, eS)) =>
        if (bC > inpL / 2) ("1" + gS, "0" + eS)
        else ("0" + gS, "1" + eS)
    }

    Integer.parseInt(gamma, 2) * Integer.parseInt(epsilon, 2)
  }

  val res = powerCalc(p1In)

  println(s"Result: $res")

}
