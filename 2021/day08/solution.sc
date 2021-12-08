import scala.io.Source

final case class Entry(signalPatterns: List[String], outputValues: List[String])

object Solution {

  def main(args: Array[String]): Unit = {
    println(partOne())
  }

  def partOne(): Int = {
    val uniqueLengths = Set(2, 3, 4, 7)
    entries.flatMap(entry => entry.outputValues.filter(str => uniqueLengths.contains(str.length))).length
  }

  def entries: Iterator[Entry] = {
    input
      .map(line => line.split('|'))
      .map(arr =>
        Entry(
          signalPatterns = arr.head.trim.split(' ').toList,
          outputValues = arr.last.trim.split(' ').toList
        )
      )
  }

  private def input: Iterator[String] = {
    Source
      .fromResource("./input.txt")
      .getLines()
  }
}
