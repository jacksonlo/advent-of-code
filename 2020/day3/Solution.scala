import scala.collection.mutable.ListBuffer
import scala.io.Source

object Solution extends App {

  println(partOne(3, 1));


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
    while (row < treeMap.length - 1) {
      col = (col + right) % rowLength;
      row = math.min(row + down, treeMap.length - 1);

      if (treeMap(row)(col)) {
        trees += 1
      }
    }
    trees
  }

}
