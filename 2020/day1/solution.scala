import scala.collection.mutable
import scala.io.Source

object Solution extends App {
  val target = 2020;
  val numberToDiff = new mutable.HashMap[Int, Int]();
  Source.fromResource("input.txt").getLines().map( line => line.toInt).foreach(number => numberToDiff(number) = target - number);
//  println(numberToDiff);

  var answer: Option[Int] = null;
  numberToDiff.foreachEntry((number, diff) => {
    if (numberToDiff.contains(diff)) {
      answer = Some(number * diff);
    }
  });

  println(answer);
}
