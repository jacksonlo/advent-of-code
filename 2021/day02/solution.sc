import scala.io.Source

sealed trait Direction
object Direction {
  case object Up extends Direction
  case object Down extends Direction
  case object Forward extends Direction
}

final case class Command(direction: Direction, amount: Int)

final case class Position(horizontal: Int, depth: Int, aim: Int)

object Solution {
  def main(args: Array[String]): Unit = {
    println(partOne())
    println(partTwo())
  }

  def partOne(): Int = {
    val finalPosition = getInput().foldLeft(Position(0, 0, 0))((position, command) => {
      command.direction match {
        case Direction.Up => position.copy(depth = position.depth - command.amount)
        case Direction.Down => position.copy(depth = position.depth + command.amount)
        case Direction.Forward => position.copy(horizontal = position.horizontal + command.amount)
      }
    })
    finalPosition.depth * finalPosition.horizontal
  }

  def partTwo(): Int = {
    val finalPosition = getInput().foldLeft(Position(0, 0, 0))((position, command) => {
      command.direction match {
        case Direction.Up => position.copy(aim = position.aim - command.amount)
        case Direction.Down => position.copy(aim = position.aim + command.amount)
        case Direction.Forward => position.copy(
          horizontal = position.horizontal + command.amount,
          depth = position.depth + position.aim * command.amount
        )
      }
    })
    finalPosition.depth * finalPosition.horizontal
  }

  private def getInput(): Iterator[Command] = {
    Source.fromResource("./input.txt")
      .getLines().map(_.split(" ")).map(pair => {
      val direction = pair.head match {
        case "up" => Direction.Up
        case "down" => Direction.Down
        case "forward" => Direction.Forward
      }
      Command(direction, pair(1).toInt)
    })
  }
}
