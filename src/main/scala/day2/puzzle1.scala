package day2

object puzzle1 extends App {
  val p1In = inputs.p1.split("\\n").filter(_.nonEmpty).map(_.trim()).toList

  println(s"Length of input: ${p1In.length}")

  case class Bearings(depth: Int, hor: Int) {
    def empty                     = Bearings(0, 0)
    def fromD(depthF: Int => Int) = Bearings(depthF(depth), hor)
    def fromH(horF: Int => Int)   = Bearings(depth, horF(hor))
  }

  object Bearings {
    def empty: Bearings = Bearings(0, 0)
  }

  def genB(s: String, b: Bearings): Bearings = s match {
    case s"forward $i" => b.fromH(i.toInt + _)
    case s"down $i"    => b.fromD(_ + i.toInt)
    case s"up $i"      => b.fromD(_ - i.toInt)
    case _             => b
  }

  val res = p1In.foldLeft(Bearings.empty) {
    case (b, s) =>
      genB(s, b)
  }

  println(s"Depth: ${res.depth}\\nHorizontal: ${res.hor}")
  println(s"Multipled value: ${res.depth * res.hor}")
}
