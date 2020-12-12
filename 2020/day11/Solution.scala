import scala.io.Source

object Solution {

  def main(args: Array[String]): Unit = {
    println(partOne());
    println(partTwo());
  }

  val EmptySeat = 'L';
  val OccupiedSeat = '#';
  val Floor = '.';

  val Directions: Array[(Int, Int)] = Array(
    (-1, 0),
    (1, 0),
    (0, -1),
    (0, 1),
    (-1, -1),
    (-1, 1),
    (1, -1),
    (1, 1),
  );

  def partOne(): Int = {
    var seatMap = buildSeatMap();
    var lastSeatMap: Array[Array[Char]] = Array();
    while (!isSame(seatMap, lastSeatMap)) {
      val newSeatMap = applyRules(seatMap, 4, getAdjacentSeats);
      lastSeatMap = seatMap;
      seatMap = newSeatMap;
    }

    seatMap.map(_.count(c => c == OccupiedSeat)).sum
  }

  def partTwo(): Int = {
    var seatMap = buildSeatMap();
    var lastSeatMap: Array[Array[Char]] = Array();
    while (!isSame(seatMap, lastSeatMap)) {
      val newSeatMap = applyRules(seatMap, 5, getVisibleAdjacentSeats);
      lastSeatMap = seatMap;
      seatMap = newSeatMap;
    }

    seatMap.map(_.count(c => c == OccupiedSeat)).sum
  }

  private def isSame(a: Array[Array[Char]], b: Array[Array[Char]]): Boolean = {
    if (a.length != b.length) {
      return false;
    }
    for(i <- a.indices) {
      for(j <- b(i).indices) {
        if (a(i)(j) != b(i)(j)) {
          return false;
        }
      }
    }
    true;
  }

  private def applyRules(seatMap: Array[Array[Char]], minOccupied: Int, adjacentFunc: (Array[Array[Char]], Int, Int) => Int): Array[Array[Char]] = {
    val newSeatMap = seatMap.map(_.clone);
    for(i <- seatMap.indices) {
      for(j <- seatMap(i).indices) {
        val currentSeat = seatMap(i)(j);
        if (currentSeat == EmptySeat) {
          val occupied = adjacentFunc(seatMap, i, j);
          if (occupied == 0) {
            newSeatMap(i)(j) = OccupiedSeat;
          }
        } else if (currentSeat == OccupiedSeat) {
          val occupied = adjacentFunc(seatMap, i, j);
          if (occupied >= minOccupied) {
            newSeatMap(i)(j) = EmptySeat;
          }
        }
      }
    }
    newSeatMap;
  }

  private def getAdjacentSeats(seatMap: Array[Array[Char]], row: Int, col: Int): Int = {
    Directions.map(d => {
      val rowDelta = d._1;
      val colDelta = d._2;
      val newRow = row + rowDelta;
      val newCol = col + colDelta;
      var result = 0;
      if (newRow >= 0 && newRow < seatMap.length) {
        if (newCol >= 0 && newCol < seatMap(row).length) {
          if (seatMap(newRow)(newCol) == OccupiedSeat) {
            result = 1;
          }
        }
      }
      result;
    }).sum
  }

  private def getVisibleAdjacentSeats(seatMap: Array[Array[Char]], row: Int, col: Int): Int = {
    Directions.map(d => {
      var result = 0;

      val rowDelta = d._1;
      val colDelta = d._2;

      var stop = false;
      var newRow = row + rowDelta;
      var newCol = col + colDelta;
      while (!stop && newRow >= 0 && newRow < seatMap.length && newCol >= 0 && newCol < seatMap(row).length) {
        if (seatMap(newRow)(newCol) != Floor) {
          result = if (seatMap(newRow)(newCol) == OccupiedSeat) 1 else 0;
          stop = true;
        }
        newRow += rowDelta;
        newCol += colDelta;
      }
      result;
    }).sum
  }

  private def buildSeatMap(): Array[Array[Char]] = {
    parseFile().map(line => line.toCharArray).toArray;
  }

  private def printSeatMap(seatMap: Array[Array[Char]]): Unit = {
    println("---------------------------------------")
    println(seatMap.map(_.mkString).mkString("\n"));
  }

  private def parseFile(): Iterator[String] = {
    Source.fromResource("2020/day11/input.txt")
      .getLines()
  }
}
