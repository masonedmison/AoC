package day6

import ext.lib._
import cats.implicits._

object part12 extends App {
  def parseInputToM(f: String): Map[Int, Long] =
    ext.lib
      .readAll(f)
      .split(",")
      .map(_.toInt)
      .toList
      .groupBy(identity)
      .mapValues(_.length.toLong)
      .toMap |+| Map.from((0 to 8) zip List.fill(9)(0L))

  def simFish(days: Int, fish: Map[Int, Long]): Map[Int, Long] = {
    if (days == 0) fish
    else {
      val zC = fish.getOrElse(0, 0L)
      val nextF = (fish.toList.sorted.unzip match {
        case (ks, vs) =>
          (ks zip ((vs slice (1, 9)).toList :+ 0L)).toMap
      }).updatedWith(6) {
          case Some(v) => Some(v + zC)
          case _       => None
        }
        .updatedWith(8) {
          case Some(v) => Some(v + zC)
          case _       => None
        }
      simFish(days - 1, nextF)
    }
  }

  locally {
    assertEquals(
      simFish(80, parseInputToM("day6-example.txt")).values.sum,
      5934,
      "day 6 example with 80 days"
    )
    assertEquals(
      simFish(256, parseInputToM("day6-example.txt")).values.sum,
      26984457539L,
      "day 6 example with 256 days"
    )
    assertEquals(
      simFish(80, parseInputToM("puzzle6-input.txt")).values.sum,
      353274,
      "day 6 with actual input with 80 days"
    )
    assertEquals(
      simFish(256, parseInputToM("puzzle6-input.txt")).values.sum,
      1609314870967L,
      "day 6 with actual input with 256 days"
    )
  }

}
