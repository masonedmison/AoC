package day2

object puzzle2 extends App {
  val p1In = inputs.p1.split("\\n").filter(_.nonEmpty).map(_.trim()).toList

  println(s"Length of input: ${p1In.length}")

  case class Bearings(depth: Int, hor: Int, aim: Int) {
    def empty        = Bearings(0, 0, 0)
    def down(i: Int) = Bearings(depth, hor, aim + i)
    def up(i: Int)   = Bearings(depth, hor, aim - i)
    def forward(i: Int) = {
      Bearings(
        depth + (aim * i),
        hor + i,
        aim
      )
    }
  }

  object Bearings {
    def empty: Bearings = Bearings(0, 0, 0)
  }

  def genB(s: String, b: Bearings): Bearings = s match {
    case s"forward $i" => b.forward(i.toInt)
    case s"down $i"    => b.down(i.toInt)
    case s"up $i"      => b.up(i.toInt)
    case _             => b
  }

  val res = p1In.foldLeft(Bearings.empty) { case (b, s) => genB(s, b) }

  println(s"Depth: ${res.depth}\\nHorizontal: ${res.hor}")
  println(s"Multipled value: ${res.depth * res.hor}")
}
