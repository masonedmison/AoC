package day5

/**
  * alternate approach without considering vector or state machine
  */
object Part12V2 extends App {
  case class Point(x: Int, y: Int)
  object Point {
    def fromStringPair(s: String): (Point, Point) = s match {
      case s"$x1,$y1 -> $x2,$y2" => (Point(x1.toInt, y1.toInt), Point(x2.toInt, y2.toInt))
      case _                     => throw new Exception(s"Unable to parse pair of Points from $s")
    }
    def intermediatePs(p1: Point, p2: Point, considerDiag: Boolean = false): List[Point] = (p1, p2) match {
      case (Point(x1, y1), Point(x2, y2)) =>
        if (x1 == x2) (Math.min(y1, y2) to Math.max(y1, y2)).toList.map(Point(x1, _))
        else if (y1 == y2) (Math.min(x1, x2) to Math.max(x1, x2)).toList.map(Point(_, y1))
        else if (considerDiag && (Math.abs(x1 - x2) == Math.abs(y1 - y2))) {
          val xs = Math.min(x1, x2) to Math.max(x1, x2)
          val ys = Math.min(y1, y2) to Math.max(y1, y2)
          val z  = if (x1 > x2 != y1 > y2) (xs zip ys.reverse) else (xs zip ys)
          z.map { case (x, y) => Point(x, y) }.toList
        } else List.empty[Point]
    }

    def countGT(ps: List[(Point, Point)], n: Int, considerDiag: Boolean): Int =
      ps.flatMap { case (p1, p2) => Point.intermediatePs(p1, p2, considerDiag) }
        .groupMapReduce(identity)(_ => 1)(_ + _)
        .count { case (_, v) => v >= n }
  }

  def parseInput: List[(Point, Point)] =
    ext.lib
      .readAll("puzzle5-input.txt")
      // .readall("day5-example.txt")
      .split("\\n")
      .map(_.trim)
      .filter(_.nonEmpty)
      .map(Point.fromStringPair)
      .toList

  val inPointPairs = parseInput

  val wod = Point.countGT(inPointPairs, 2, false)
  val wd  = Point.countGT(inPointPairs, 2, true)

  println(s"Without diagonal $wod")
  println(s"With diagonal $wd")

}
