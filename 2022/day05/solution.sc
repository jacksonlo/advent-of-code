import scala.io.Source

final case class Move(count: Int, fromColIndex: Int, toColIndex: Int)

object Solution {

  def main(args: Array[String]): Unit = {
    println(partOne())
    println(partTwo())
  }

  def partOne(): String = {
    execute(false)
  }

  def partTwo(): String = {
    execute(true)
  }

  def execute(moveAsStack: Boolean): String = {
    val finalStack = moves.foldLeft(startingStacks)((stacks, move) => {
      var cratesToMove = stacks(move.fromColIndex).take(move.count)
      if (!moveAsStack) {
        cratesToMove = cratesToMove.reverse
      }

      stacks
        .updated(move.toColIndex, cratesToMove ++ stacks(move.toColIndex))
        .updated(move.fromColIndex, stacks(move.fromColIndex).drop(move.count))
    })
    finalStack.map(_.head).mkString
  }

  def startingStacks: List[List[Char]] = {
    val rawRows = input.takeWhile(_.trim != "").toList.dropRight(1)
    val rows = rawRows.map(row => row.grouped(4).map(x => x.trim.find(c => c.isLetter)).toList).toList
    val columns = rows.transpose.map(column => column.filter(_.isDefined).map(_.get))
    columns
  }

  def moves: List[Move] = {
    val splitIndex = input.indexWhere(_.trim == "")
    val movesSection = input.toList.drop(splitIndex + 1)
    movesSection.map(line => {
      val nums = line.split(" ").filter(_.forall(_.isDigit)).map(_.toInt)
      Move(nums(0), nums(1) - 1, nums(2) - 1)
    })
  }

  private def input: Iterator[String] = {
    Source
      .fromResource("./input.txt")
      .getLines()
  }
}