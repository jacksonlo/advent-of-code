import scala.io.Source

object Solution {

  def main(args: Array[String]): Unit = {
    println(partOne())
    println(partTwo())
  }

  def partOne(): Int = {
    findMarker(4)
  }

  def partTwo(): Int = {
    findMarker(14)
  }

  private def findMarker(markerLength: Int): Int = {
    input.map(line => {
      line.sliding(markerLength).takeWhile(x => x.distinct != x).length + markerLength
    }).sum
  }

  private def input: Iterator[String] = {
    Source
      .fromResource("./input.txt")
      .getLines()
  }
}