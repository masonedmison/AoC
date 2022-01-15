package day4

import cats.implicits._

object puzzle12 extends App {
  type Board = Vector[Int]

  case class BoardEntry(id: Int, indices: List[Int])

  object Board {
    val r = (0 to 4).toList
    val c = List(0, 5, 10, 15, 20)
    val rowConds = List(
      r,
      r.map(_ + 5),
      r.map(_ + 10),
      r.map(_ + 15),
      r.map(_ + 20)
    )
    val colConds = List(
      c,
      c.map(_ + 1),
      c.map(_ + 2),
      c.map(_ + 3),
      c.map(_ + 4)
    )
    def winner(b: Board): Boolean = {

      val rowSums = rowConds.foldRight(List.empty[Int]) { case (l, acc) => l.map(b(_)).sum :: acc }
      val colSums = colConds.foldRight(List.empty[Int]) { case (l, acc) => l.map(b(_)).sum :: acc }
      rowSums.exists(_ == -5) || colSums.exists(_ == -5)
    }
  }

  val (_, idToBoard, numToBoardEnt) = inputs.boards
    .split("\\W\\n")
    .map(_.trim)
    .filter(_.nonEmpty)
    .foldLeft((0, Map.empty[Int, Board], Map.empty[Int, List[BoardEntry]])) {
      case ((curId, iToBoard, numToBoardEnt), sBlock) =>
        val board = sBlock.split("\\n").flatMap(_.split("\\s").filter(_.nonEmpty)).map(_.toInt)
        (
          curId + 1,
          iToBoard + (curId -> board.toVector),
          numToBoardEnt |+| board
            .zip(0 to board.length - 1)
            .groupBy { case (el, _) => el }
            .map { case (el, arrI) => el -> List(BoardEntry(curId, arrI.toList.map(_._2))) }
        )
    }

  def updateBoards(
      boards: Map[Int, Board],
      num: Int,
      alreadyWon: List[Int]
  ): (Map[Int, Board], Option[List[(Int, Board)]]) = {
    val boardEnts = numToBoardEnt(num)
    val newBoards = boardEnts.foldLeft(boards) {
      case (accM, be) =>
        val indsToRep = be.indices
        val b         = accM(be.id)
        val upB       = indsToRep.toVector.foldRight(b) { case (i, v) => v.updated(i, -1) }
        accM.updated(be.id, upB)
    }
    boardEnts
      .map(_.id)
      .filter(i => !alreadyWon.contains(i))
      .sorted
      .filter { i =>
        Board.winner(newBoards(i))
      } match {
      case Nil => (newBoards, None)
      case l   => (newBoards, Some(l.map(i => (i, newBoards(i)))))
    }
  }
  def exhaustToDraw(toDraw: List[Int], boards: Map[Int, Board]) = {
    def go(
        remDraw: List[Int],
        boardsR: Map[Int, Board],
        winningBs: List[(Int, (Int, Board))]
    ): List[(Int, (Int, Board))] =
      remDraw match {
        case Nil =>
          winningBs
        case h :: t =>
          updateBoards(boardsR, h, winningBs.map(e => e._2._1)) match {
            case (newB, Some(l)) =>
              go(t, newB, l.map((h, _)) ++ winningBs)
            case (newB, None) => go(t, newB, winningBs)
          }
      }
    go(toDraw, boards, List.empty)
  }

  val winningB = exhaustToDraw(inputs.toDraw, idToBoard)

  winningB match {
    case l @ h :: t =>
      val (fi, f)  = (l.last._1, l.last._2._2)
      val (li, la) = (l.head._1, l.head._2._2)
      println(s"Final score first:  ${f.filter(_ != -1).sum * fi}")
      println(s"Final score last:  ${la.filter(_ != -1).sum * li}")
    case _ =>
      println("No winning board found")
  }

}
