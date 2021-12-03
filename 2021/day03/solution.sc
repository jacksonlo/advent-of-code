import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer
import scala.io.Source

object Solution {
  val Zero = '0'
  val One = '1'

  final case class Count(index: Int, zero: Int, one: Int) {
    def mostCommon(): Char = if (zero > one) Zero else One
    def leastCommon(): Char = if (zero <= one) Zero else One
  }

  def main(args: Array[String]): Unit = {
    println(partOne())
    println(partTwo())
  }

  def partOne(): Int = {
    val indexCounts = getIndexCounts(input.toList)
    val gammaRate = indexCounts.map(_.mostCommon()).mkString
    val epsilonRate = indexCounts.map(_.leastCommon()).mkString

    Integer.parseInt(gammaRate, 2) * Integer.parseInt(epsilonRate, 2)
  }

  def partTwo(): Int = {
    val bitStrings = input.toList
    val oxygenRating = bitFilter((c: Count) => c.mostCommon())(bitStrings, 0).head
    val co2Rating = bitFilter((c: Count) => c.leastCommon())(bitStrings, 0).head

    Integer.parseInt(oxygenRating, 2) * Integer.parseInt(co2Rating, 2)
  }

  @tailrec
  private def bitFilter(f: Count => Char)(bitStrings: List[String], index: Int): List[String] = {
    if (bitStrings.length == 1) {
      return bitStrings
    }

    val indexCounts = getIndexCounts(bitStrings)
    var newBitStrings = bitStrings
    val target = f(indexCounts(index))
    for (bitString <- bitStrings) {
      if (newBitStrings.length != 1 && bitString.charAt(index) != target) {
        newBitStrings = newBitStrings.filterNot(x => x == bitString)
      }
    }
    bitFilter(f)(newBitStrings, index + 1)
  }

  private def getIndexCounts(value: Iterable[String]): List[Count] = {
    val length = value.take(1).toList.head.length
    val initial = List.range(0, length).map(_ => Count(0, 0, 0))
    value.foldLeft(initial)((list, bitString) => {
      list.zipWithIndex.map {
        case (count: Count, index: Int) =>
          if (bitString.charAt(index) == Zero)
            count.copy(index = index, zero = count.zero + 1)
          else count.copy(index = index, one = count.one + 1)
      }
    })
  }

  private def input: Iterator[String] = {
    Source
      .fromResource("./input.txt")
      .getLines()
  }
}
