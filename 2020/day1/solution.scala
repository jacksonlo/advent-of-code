import scala.collection.mutable
import scala.io.Source

object Solution extends App {

  partOne
  partTwo

  def partOne: Unit = {
    val target = 2020;
    val numberToDiff = new mutable.HashMap[Int, Int]();
    Source.fromResource("2020/day1/input.txt").getLines().map(line => line.toInt).foreach(number => numberToDiff(number) = target - number);
    //  println(numberToDiff);

    var answer: Option[Int] = null;
    numberToDiff.foreachEntry((number, diff) => {
      if (numberToDiff.contains(diff)) {
        answer = Some(number * diff);
      }
    });

    println(answer);
  }

  def partTwo: Unit = {
    val target = 2020;

    val numbers = Source.fromResource("2020/day1/input.txt").getLines().map(line => line.toInt).toList
    val numberToDiff = new mutable.HashMap[Int, Int]();
    numbers.foreach(number => numberToDiff(number) = target - number);
    //  println(numberToDiff);

    var answer: Option[Int] = null;
    numberToDiff.foreachEntry((number, diff) => {
      // Loop over again to find a pairing that adds up to this diff as the target
      val subTarget = diff;

      val subNumberToDiff = new mutable.HashMap[Int, Int]();
      numbers.foreach(num => {
        if (num != number) {
          subNumberToDiff(num) = subTarget - num
        }
      });

      subNumberToDiff.foreachEntry((num2, subDiff) => {
        if (num2 != subDiff && subNumberToDiff.contains(subDiff)) {
          answer = Some(number * num2 * subDiff)
        }
      })
    });

    println(answer);
  }
}
