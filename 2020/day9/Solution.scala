import scala.collection.View
import scala.io.Source

object Solution {

  def main(args: Array[String]): Unit = {
    val cypher = parseFile().map(line => line.toLong).toVector
    val partOneResult = partOne(cypher, 25);

    println(partOneResult);
    println(partTwo(cypher, partOneResult))
  }

  def partOne(cypher: Vector[Long], preambleSize: Int): Long = {
    var currentIndex: Int = preambleSize;
    cypher.slice(preambleSize, cypher.size).takeWhile((cypherValue: Long) => {
      val startIndex = currentIndex-preambleSize;
      val result = isNextNumberValid(cypher.view.slice(startIndex, currentIndex), cypherValue)
      if (result) {
        currentIndex += 1
      }
      result
    })

    cypher(currentIndex)
  }

  def partTwo(cypher: Vector[Long], invalidNumber: Long): Long = {
    val matrix = Array.fill[Long](cypher.size, cypher.size)(0);

    // Populate matrix
    matrix.view.zipWithIndex.foreach((value: (Array[Long], Int)) => {
      val row = value._1;
      val rowIndex = value._2;
      for (colIndex <- rowIndex + 1 until matrix(rowIndex).length) {
        val prevCol = matrix(rowIndex)(colIndex - 1)
        matrix(rowIndex)(colIndex) = prevCol + cypher(colIndex)
        if (prevCol == 0) {
          matrix(rowIndex)(colIndex) += cypher(rowIndex)
        }
      }
    })

    // Find cell and return smallest + largest from the row
    matrix.view.zipWithIndex.toVector.takeWhile((value: (Array[Long], Int)) => {
      val row = value._1;
      val rowIndex = value._2;

      val colIndex = row.indexOf(invalidNumber);
      if (colIndex != -1) {
        val slicedView = cypher.view.slice(rowIndex - 1, colIndex)
        return slicedView.min + slicedView.max
      }
      true
    })
    throw new Error("No contiguous seq found that adds up to target")
  }

  private def isNextNumberValid(preamble: View[Long], cypherValue: Long): Boolean = {
    val diffMap = preamble.toVector.map((value: Long) => (value -> (cypherValue - value))).toMap
    var contains = false;
    diffMap.takeWhile((keyValue: (Long, Long)) => {
      val diff = keyValue._2;
      val result = diffMap.contains(diff);
      if (result) {
        contains = result
      }
      !result
    })
    contains
  }

  private def parseFile(): Iterator[String] = {
    Source.fromResource("2020/day9/input.txt")
      .getLines()
  }
}
