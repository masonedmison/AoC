package day1

import inputs._

object puzzle1solution extends App {
  val p1In = puzzle1.split("\\n").filter(_.nonEmpty).map(_.trim().toInt).toList

  println(p1In.length)

  // Note different values return  between foldRight and foldLeft...
  val (numInc, acc) = p1In.tail.foldLeft((0, p1In.head)) {
    case ((acc, prev), i) =>
      if (i > prev) (acc + 1, i)
      else (acc, i)
  }

  println(s"Number inc: $numInc LastAcc: $acc")

}
