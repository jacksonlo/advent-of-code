import scala.io.Source

object Solution {

  def main(args: Array[String]): Unit = {
    val input = parseFile().toArray;

    val earliestDeparture: Double = input(0).toDouble;
    val busIds: Array[Int] = input(1).split(",").filter(_ != "x").map(_.toInt);
    println(partOne(earliestDeparture, busIds));

    val busSchedule = input(1).split(",")
      .zipWithIndex
      .map(_.swap)
      .toMap
      .filter(_._2 != "x")
      .transform((k, v) => v.toInt)
    println(partTwo(busSchedule));
  }

  def partOne(earliestDeparture: Double, busIds: Array[Int]): Int = {
    val earliestBusId = busIds.reduceLeft((closestBusId: Int, busId: Int) => {
      val earliest = Math.ceil(earliestDeparture / closestBusId) * closestBusId;
      val current = Math.ceil(earliestDeparture / busId) * busId;
      if (current < earliest) busId else closestBusId;
    });
    val minutesWaited = Math.ceil(earliestDeparture / earliestBusId) * earliestBusId - earliestDeparture;
    earliestBusId * minutesWaited.toInt
  }

  def partTwo(busSchedule: Map[Int, Int]): Long = {
    /*
    (t + 0) mod 7 == 0
    (t + 1) mod 13 == 0
    (t + 4) mod 59 == 0
    (t + 6) mod 31 == 0
    (t + 7) mod 19 == 0
     */
    val scheduleKeys = busSchedule.keys;
    var time: Long = 0;
    var nextCheck: Long = 1;
    for(i <- scheduleKeys) {
      val busId = busSchedule(i);
      while((time + i) % busId != 0) {
        time += nextCheck;
      }
      nextCheck *= busId;
    }
    time;
  }

  private def parseFile(): Iterator[String] = {
    Source.fromResource("2020/day13/input.txt")
      .getLines()
  }
}
