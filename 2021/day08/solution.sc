import scala.io.Source

final case class Entry(signalPatterns: List[String], outputValues: List[String])

object Solution {

  def main(args: Array[String]): Unit = {
    println(partOne())
    println(partTwo())
  }

  def partOne(): Int = {
    val uniqueLengths = Set(2, 3, 4, 7)
    entries.flatMap(entry => entry.outputValues.filter(str => uniqueLengths.contains(str.length))).length
  }

  def partTwo(): Int = {
//    entries.map(entry => {
//      val entryMapping: Map[String, Int] = entry.signalPatterns.map(signal => {
//        val convertedSignal = signal.map(c => mapping(c)).sorted
//        signal -> displayMap(convertedSignal)
//      }).toMap
//
//      val output = entry.outputValues.map(output => entryMapping(output)).mkString.toInt
//      output
//    }).sum
    entries.map(entry => {
      val x = codeToNumber.map((k, v) => )
      val lengthToNumber = Map(
        0 -> -1,
        1 -> -1,
        2 -> 1,
        3 -> 7,
        4 -> 4,
        5 -> -1,
        6 -> -1,
        7 -> 8,
        8 -> -1,
        9 -> -1,
      )

    })
  }

  private def codeToNumber: Map[String, Int] = {
    Map(
      "abcefg" -> 0,
      "cf" -> 1,
      "acdeg" -> 2,
      "acdfg" -> 3,
      "bcdf" -> 4,
      "abdfg" -> 5,
      "abdefg" -> 6,
      "acf" -> 7,
      "abcdefg" -> 8,
      "abcdfg" -> 9,
    )
  }

  def entries: Iterator[Entry] = {
    input
      .map(line => line.split('|'))
      .map(arr =>
        Entry(
          signalPatterns = arr.head.trim.split(' ').map(_.sorted).toList,
          outputValues = arr.last.trim.split(' ').map(_.sorted).toList
        )
      )
  }

  private def input: Iterator[String] = {
    Source
      .fromResource("./input.txt")
      .getLines()
  }
}
