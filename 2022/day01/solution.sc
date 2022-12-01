import scala.io.Source

object Solution {

  def main(args: Array[String]): Unit = {
    println(partOne())
    println(partTwo())
  }

  def partOne(): Int = {
    _elfCalories().max
  }

  def partTwo(): Int = {
    _elfCalories().sorted.takeRight(3).sum
  }

  def _elfCalories(): Vector[Int] = {
    input.foldLeft(Vector(0)) {
      case (vec, calories) =>
        if (calories != "") {
          vec.updated(vec.size - 1, vec.last + calories.toInt)
        } else {
          vec :+ 0
        }
    }
  }

  private def input: Iterator[String] = {
    Source
      .fromResource("./input.txt")
      .getLines()
  }
}