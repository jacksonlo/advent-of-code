import scala.collection.mutable.ListBuffer
import scala.io.Source

object Solution {

  case class Seat(row: Int, column: Int) {
    def id: Int = {
      row * 8 + column
    }
  }

  def main(args: Array[String]): Unit = {
    println(partOne());
    println(partTwo());
  }

  def partOne(): Int = {
    parseSeats().maxBy(_.id).id
  }

  def partTwo(): Int = {
    val seatIds = parseSeats()
      .map(seat => seat.id)
      .toSet

    var mySeat: Int = 0;
    seatIds.foreach(seatId => {
      if (seatIds.contains(seatId + 2) && !seatIds.contains(seatId + 1)) {
        mySeat = seatId + 1
      } else if (seatIds.contains(seatId - 2) && !seatIds.contains(seatId - 1)) {
        mySeat = seatId - 1
      }
    })
    mySeat
  }

  private def parseSeats(): Iterator[Seat] = {
    parseFile().map(line => {
      val frontBack = line.slice(0, 7);
      val leftRight = line.slice(7, 10);

      val row = decodeSeat(frontBack, 0, 127, 'F', 'B')
      val column = decodeSeat(leftRight, 0, 7, 'L', 'R')

      Seat(row, column)
    })
  }

  private def parseFile(): Iterator[String] = {
    Source.fromResource("2020/day5/input.txt")
      .getLines()
  }

  private def testSeats(): Array[String] = {
    /*
    FBFBBFFRLR: row 44, column 5, seat ID 357.
    BFFFBBFRRR: row 70, column 7, seat ID 567.
    FFFBBBFRRR: row 14, column 7, seat ID 119.
    BBFFBBFRLL: row 102, column 4, seat ID 820.
     */
    Array("FBFBBFFRLR", "BFFFBBFRRR", "FFFBBBFRRR", "BBFFBBFRLL")
  }

  private def decodeSeat(line: String, start: Int, end: Int, lowerHalfChar: Char, upperHalfChar: Char): Int = {
    var _start = start
    var _end = end
    line.foreach(char => {
      if (char == lowerHalfChar) {
        _end -= Math.ceil((_end - _start.toDouble) / 2).toInt
      } else if(char == upperHalfChar) {
        _start += Math.ceil((_end - _start.toDouble) / 2).toInt
      } else {
        throw new Error("Unexpected char")
      }
    })
    if (line.last == lowerHalfChar) {
      _start
    } else {
      _end
    }
  }

}
