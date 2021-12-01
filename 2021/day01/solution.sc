import scala.io.Source

object Solution {
  def main(args: Array[String]): Unit = {
    println(partOne());
  }

  def partOne(): Int = {
    parseFile().sliding(2).foldLeft(0)((total, depthPair) => {
      val first: Int = depthPair(0).toInt
      val second: Int = depthPair(1).toInt
      if (second > first) total + 1 else total
    })
  }

  private def parseFile(): Iterator[String] = {
    Source.fromResource("./input.txt")
      .getLines()
  }
}
