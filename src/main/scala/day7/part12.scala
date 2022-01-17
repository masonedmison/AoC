package day7

import ext.lib.assertEquals

object part12 extends App {
  def parseInput(f: String) =
    ext.lib
      .readAll(f)
      .split(",")
      .map(_.toInt)
      .toList

  def minMaxRange(x: Int, y: Int): List[Int] =
    List(x, y).sorted match {
      case min :: max :: Nil =>
        (min to max).toList
      case _ => throw new Exception("Inconcievable!")
    }

  def fuelByN(n: Int, pos: List[Int], stepF: (Int, Int) => Int): Int =
    pos.map(i => stepF(i, n)).sum

  def search(pos: List[Int], stepF: (Int, Int) => Int) = {
    def loop(moveF: Int => Int, n: Int, prevF: Int): Int = {
      val nextF = fuelByN(n, pos, stepF)
      if (nextF < prevF) {
        loop(moveF, moveF(n), nextF)
      } else prevF
    }
    val avg   = pos.sum / pos.length
    val initF = fuelByN(avg.floor.toInt, pos, stepF)

    val bestL = loop(_ - 1, avg - 1, initF)
    val bestH = loop(_ + 1, avg + 1, initF)
    if (bestL < bestH) bestL
    else bestH
  }

  locally {
    assertEquals(
      search(parseInput("puzzle7-input.txt"), (i, n) => Math.abs(i - n)),
      336040,
      "Full puzzle input (part1)"
    )
    assertEquals(
      search(parseInput("day7-example.txt"), (i, n) => minMaxRange(1, Math.abs(n - i)).sum),
      168,
      "Example puzzle input (part2)"
    )
    assertEquals(
      search(parseInput("puzzle7-input.txt"), (i, n) => minMaxRange(1, Math.abs(n - i)).sum),
      94813675,
      "Full puzzle input (part2)"
    )
  }
}
