import scala.io.Source

object Solution {
  def main(args: Array[String]): Unit = {
    println(partOne())
    println(partTwo())
  }

  def partOne(): Int = {
    getInput().sliding(2).foldLeft(0)((total, depthPair) => {
      val first: Int = depthPair(0)
      val second: Int = depthPair(1)
      if (second > first) total + 1 else total
    })
  }

  def partTwo(): Int = {
    val sums = getInput().sliding(3).map(group => group.sum)
    sums.sliding(2).foldLeft(0)((total, depthSum) => {
      val first: Int = depthSum(0)
      val second: Int = depthSum(1)
      if (second > first) total + 1 else total
    })
  }

  private def getInput(): Iterator[Int] = {
    Source.fromResource("./input.txt")
      .getLines().map(_.toInt)
  }

  private def getExampleInput(): Iterator[Int] = {
    List(199, 200, 208, 210, 200, 207, 240, 269, 260, 263).iterator
  }
}
