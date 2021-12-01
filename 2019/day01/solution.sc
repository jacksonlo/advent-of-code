import scala.annotation.tailrec
import scala.io.Source

object Solution {
  def main(args: Array[String]): Unit = {
    println(partOne())
    println(partTwo())
  }

  def partOne(): Int = {
    getInput().map(mass => {
      (mass / 3) - 2
    }).sum
  }

  def partTwo(): Int = {
    getInput().map(mass => _recurse(mass, 0)).sum
  }

  @tailrec
  def _recurse(value: Int, total: Int): Int = {
    val newValue = value / 3 - 2
    if (newValue <= 0) {
      total
    } else {
      _recurse(newValue, total + newValue)
    }
  }

  private def getInput(): Iterator[Int] = {
    Source.fromResource("./input.txt")
      .getLines().map(_.toInt)
  }
}
