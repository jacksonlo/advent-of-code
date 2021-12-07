import scala.io.Source


object Solution {

  def main(args: Array[String]): Unit = {
    println(partOne())
    println(partTwo())
  }

  def partOne(): Int = {
    val position = numbers.sorted.drop(numbers.length/2).head
    numbers.map(num => (num - position).abs).sum
  }

  def partTwo(): Int = {
    val range = Range.inclusive(0, numbers.max)
    range.map(num => calculateLinearFuel(numbers, num)).min
  }

  private def calculateLinearFuel(nums: Array[Int], targetPosition: Int): Int = {
    nums.map(num => {
      val n: Int = (num - targetPosition).abs
      (n*n + n) / 2
    }).sum
  }

  private def numbers: Array[Int] = {
    input.toList.head.split(",").map(_.toInt)
  }

  private def input: Iterator[String] = {
    Source
      .fromResource("./input.txt")
      .getLines()
  }
}
