import scala.collection.mutable.ListBuffer
import scala.io.Source

object Solution extends App {

  println(partOne(3, 1));
  println(partTwo());

  def parseTreeMap(): List[List[Boolean]] = {
    val TREE = '#';
    val treeMap = new ListBuffer[List[Boolean]];
    Source.fromResource("2020/day3/input.txt")
      .getLines()
      .foreach(line => {
        treeMap += line.map(c => c == TREE).toList
      })
    treeMap.toList
  }

  def partOne(right: Int, down: Int): Int = {
    val treeMap: List[List[Boolean]] = parseTreeMap()
    val rowLength: Int = treeMap.head.length;
    var col = 0;
    var row = 0;

    var trees = 0;
    while (row < treeMap.length) {
      col = (col + right) % rowLength;
      row = row + down;

      if (row < treeMap.length && treeMap(row)(col)) {
        trees += 1
      }
    }
    trees
  }

  def partTwo(): Long = {
    val one = partOne(1, 1).toLong
    val two = partOne(3, 1)
    val three = partOne(5, 1)
    val four = partOne(7, 1)
    val five = partOne(1, 2)
    println(one, two, three, four, five);
    one * two * three * four * five
  }

}
