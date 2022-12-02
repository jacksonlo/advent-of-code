import scala.io.Source

final case class Strategy(opponent: String, strategy: String) {
  val choices = List("A", "B", "C")

  def score(): Int = {
    val cs = choiceScore(strategy)
    if (opponent == strategy) {
      3 + cs
    } else if (opponent == "C" && strategy == "A") {
      6 + cs
    } else if (opponent > strategy) {
      0 + cs
    } else if (opponent == "A" && strategy == "C") {
      0 + cs
    } else {
      6 + cs
    }
  }

  def partTwoScore(): Int = {
    strategy match {
      case "A" =>
        var index = choices.indexOf(opponent) - 1
        if (index < 0) {
          index = choices.length - 1
        }
        0 + choiceScore(choices(index))
      case "B" => 3 + choiceScore(opponent)
      case "C" =>
        var index = choices.indexOf(opponent) + 1
        if (index == choices.length) {
          index = 0
        }
        6 + choiceScore(choices(index))
    }
  }

  def choiceScore(x: String): Int = {
    x match {
      case "A" => 1
      case "B" => 2
      case "C" => 3
    }
  }
}

object Solution {

  def main(args: Array[String]): Unit = {
    println(partOne())
    println(partTwo())
  }

  def partOne(): Int = {
    guide().map(s => s.score()).sum
  }

  def partTwo(): Int = {
    guide().map(s => s.partTwoScore()).sum
  }

  private def guide(): Iterator[Strategy] = {
    input.map(x => {
      val split = x.split(" ")
      val suggested = split(1) match {
        case "X" => "A"
        case "Y" => "B"
        case "Z" => "C"
      }
      Strategy(split(0), suggested)
    })
  }

  private def input: Iterator[String] = {
    Source
      .fromResource("./input.txt")
      .getLines()
  }
}