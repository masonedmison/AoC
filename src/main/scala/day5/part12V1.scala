package day5

import Math.{ max, min }

object part1 extends App {

  // thread state vs accumulate a list and build state from this list at end?
  case class FSM[S, I](run: (S, I) => S)

  case class Point(x: Int, y: Int)

  object Point {
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
    def fromStringPair(s: String): (Point, Point) = s match {
      case s"$x1,$y1 -> $x2,$y2" => (Point(x1.toInt, y1.toInt), Point(x2.toInt, y2.toInt))
      case _                     => throw new Exception(s"Unable to parse pair of Points from $s")
    }
  }

  type Diagram = Vector[Vector[Int]]

  object Diagram {
    // TODO this is pretty bad - lots of lookups
    def fromPs(d: Diagram)(ps: List[Point]): Diagram =
      ps.foldRight(d) {
        case (p, accD) =>
          val curVal       = accD(p.y)(p.x)
          val updatedInner = accD(p.y).updated(p.x, curVal + 1)
          accD.updated(p.y, updatedInner)
      }
    def empty(xDim: Int = 9, yDim: Int = 9) = Vector.fill(yDim)(Vector.fill(xDim)(0))
    def printDiagram(d: Diagram) =
      d.foreach { rowv =>
        println(s"| ${rowv.mkString(", ")}|")
      }

    def getGTE(d: Diagram)(n: Int) =
      d.zip(0 to d.length - 1).foldRight(List.empty[Point]) {
        case ((rowv, yidx), ps) =>
          rowv
            .zip(0 to rowv.length - 1)
            .filter { case (v, _) => v >= n }
            .map { case (_, xidx) => Point(xidx, yidx) }
            .toList ++ ps

      }
  }

  val inPointPairs: List[(Point, Point)] = ext.lib
    .readAll("puzzle5-input.txt")
    // .readAll("day5-example.txt")
    .split("\\n")
    .map(_.trim)
    .filter(_.nonEmpty)
    .map(Point.fromStringPair)
    .toList

  val (xDim, yDim) = inPointPairs.foldRight((0, 0)) {
    case (pair, (maxX, maxY)) =>
      pair match {
        case (Point(x1, y1), Point(x2, y2)) =>
          (max(max(x1, x2), maxX), max(max(y1, y2), maxY))

      }
  }

  val fsm = FSM[Diagram, (Point, Point)] {
    case (d, (p1, p2)) =>
      Point.intermediatePs(p1, p2, true) match {
        case ps @ _ :: _ => Diagram.fromPs(d)(ps)
        case Nil         => d
      }
    case (d, _) => d
  }

  val d = inPointPairs.foldRight(Diagram.empty(xDim + 1, yDim + 1)) {
    case (pair, d) =>
      fsm.run(d, pair)
  }

  val res = Diagram.getGTE(d)(2).length

  println(res)
}
