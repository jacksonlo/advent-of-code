import scala.collection.mutable

object Solution {

  def main(args: Array[String]): Unit = {
    val input: Array[Int] = Array(13, 0, 10, 12, 1, 5, 8);
    println(partOne(input, 2020));
    println(partOne(input, 30000000));
  }

  def partOne(startingNumbers: Array[Int], target: Int): Int = {
    val turnHistory = new mutable.HashMap[Int, (Int, Int)];
    startingNumbers.zipWithIndex.foreach(value => turnHistory(value._1) = (value._2 + 1, -1));

    var lastNumber: Int = startingNumbers.last;
    (startingNumbers.length + 1 to target).foreach((turn: Int) => {
      if (turnHistory.contains(lastNumber)) {
        val lastTwo = turnHistory(lastNumber);
        if (turnHistory(lastNumber)._2 != -1) {
          val newLastNumber = lastTwo._1 - lastTwo._2;
          turnHistory(newLastNumber) = (turn, turnHistory.getOrElse(newLastNumber, (-1, -1))._1);
          lastNumber = newLastNumber;
        } else {
          turnHistory(0) = (turn, turnHistory.getOrElse(0, (-1, -1))._1);
          lastNumber = 0;
        }
      } else {
        turnHistory(0) = (turn, turnHistory.getOrElse(0, (-1, -1))._1);
        lastNumber = 0;
      }
      if (turn % 100000 == 0) println(turn)
    });
    lastNumber;
  }
}
