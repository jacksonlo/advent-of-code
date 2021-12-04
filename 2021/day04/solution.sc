import scala.annotation.tailrec
import scala.io.Source
import scala.collection.mutable
import scala.collection.mutable.ListBuffer

final case class Board(rows: Array[Array[Int]]) {
  private val numbers = rows.flatten.toSet
  private val marked = mutable.Set[Int]()
  private val markedList = ListBuffer[Int]()
  private val transposedRows = rows.transpose

  def contains(number: Int): Boolean = numbers.contains(number)

  def mark(number: Int): Unit = {
    marked.addOne(number)
    markedList.addOne(number)
  }

  def lastMarked: Int = markedList.last

  def bingo(): Boolean = {
    val searchRows = rows.find(row => row.forall(num => marked.contains(num)))
    val searchColumns = transposedRows.find(row => row.forall(num => marked.contains(num)))
    searchRows.isDefined || searchColumns.isDefined
  }

  def score(): Int = {
    rows.flatten.filter(num => !marked.contains(num)).sum
  }

  override def toString: String = {
    val board = rows.map(row => {
      row.map(num => {
        if (marked.contains(num)) {
          num * -1
        } else {
          num
        }
      }).mkString("\t")
    }).mkString("\n")
    s"Bingo: ${bingo()}\n$board"
  }
}

object Solution {

  def main(args: Array[String]): Unit = {
    println(partOne())
    println(partTwo())
  }

  @throws[Exception]
  def partOne(): Int = {
    findFirstBingo(boards, guesses) match {
      case Some(board) => board.score() * board.lastMarked
      case _ => throw new Exception("No board found with bingo")
    }
  }

  @throws[Exception]
  def partTwo(): Int = {
    findLastBingo(boards, guesses) match {
      case Some(board) => board.score() * board.lastMarked
      case _ => throw new Exception("No board found with bingo")
    }
  }

  @tailrec
  private def findFirstBingo(boards: Array[Board], guesses: Array[Int]): Option[Board] = {
    if (guesses.length == 0) {
      return None
    }
    boards.foreach(_.mark(guesses.head))
    boards.find(_.bingo()) match {
      case Some(board) => Option(board)
      case None => findFirstBingo(boards, guesses.drop(1))
    }
  }

  @tailrec
  private def findLastBingo(boards: Array[Board], guesses: Array[Int]): Option[Board] = {
    if (guesses.length == 0) {
      return None
    }
    boards.foreach(_.mark(guesses.head))
    if (boards.length == 1 && boards.head.bingo()) {
      return Option(boards.head)
    }
    findLastBingo(boards.filterNot(_.bingo()), guesses.drop(1))
  }

  private def guesses: Array[Int] = {
    input.take(1).toList.head.split(",").map(_.toInt)
  }

  private def boards: Array[Board] = {
    val rawBoards = input.drop(2).foldLeft((Array(Array[Array[Int]]()), 0))((acc, line) => {
      val boards = acc._1
      val boardIndex = acc._2
      if (line == "") {
        (boards :+ Array(), boardIndex + 1)
      } else {
        val board = boards(boardIndex)
        val row = line.split(' ').filter(_ != "").map(_.toInt)
        boards(boardIndex) = board :+ row
        (boards, boardIndex)
      }
    })._1
    rawBoards.map(rows => Board(rows))
  }

  private def input: Iterator[String] = {
    Source
      .fromResource("./input.txt")
      .getLines()
  }
}
