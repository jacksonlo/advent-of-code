import scala.collection.mutable
import scala.io.Source

object Solution {

  def main(args: Array[String]): Unit = {
    println(partOne());
    println(partTwo());
 }

  def partOne(): Int = {
    var oneJoltDiff = 0;
    var threeJoltDiff = 1;
    parseAdapters().foldLeft(0)((lastJolt: Int, currentJolt: Int) => {
      val diff = currentJolt - lastJolt;
      if (diff == 1) {
        oneJoltDiff += 1;
      } else if (diff == 3) {
        threeJoltDiff += 1
      }
      currentJolt
    })
    oneJoltDiff * threeJoltDiff
  }

  def partTwo(): Long = {
    val parsedAdapters = parseAdapters().toVector;
    val adapters: Vector[Int] = 0 +: parsedAdapters :+ parsedAdapters.last + 3;

    val adjacencyMap = buildAdjacencyMap(adapters);
    countLeafNodes(adjacencyMap);
  }

  private def buildAdjacencyMap(adapters: Vector[Int]): Map[Int, List[Int]] = {
    val adjacencyMap = adapters.map((jolt: Int) => jolt -> new mutable.ListBuffer[Int]()).toMap;

    adapters.foldLeft(mutable.Queue[Int]())((prevJolts: mutable.Queue[Int], jolt: Int) => {
      if (prevJolts.nonEmpty) {
        while(jolt - prevJolts.head > 3) {
          prevJolts.dequeue()
        }
        prevJolts.foreach((prevJolt: Int) => {
          adjacencyMap(prevJolt).append(jolt);
        });
      }
      prevJolts.enqueue(jolt);
      prevJolts;
    })

    adjacencyMap.map(value => value._1 -> value._2.toList)
  }

  private def countLeafNodes(adjacencyMap: Map[Int, List[Int]]): Long = {
    val memoization = new mutable.HashMap[Int, Long]();
    traverse(adjacencyMap, 0, memoization)
  }

  private def traverse(adjacencyMap: Map[Int, List[Int]], currentNode: Int, memoization: mutable.HashMap[Int, Long]): Long = {
    if (adjacencyMap(currentNode).isEmpty) {
      return 1;
    }

    if(memoization.contains(currentNode)) {
      return memoization(currentNode);
    }

    val result = adjacencyMap(currentNode).map(jolt => {
      val subResult = traverse(adjacencyMap, jolt, memoization);
      memoization(jolt) = subResult;
      subResult;
    }).sum
    memoization(currentNode) = result;
    result
  }

  private def parseAdapters(): Vector[Int] = {
    parseFile().map(line => line.toInt).toVector.sorted;
  }

  private def parseFile(): Iterator[String] = {
    Source.fromResource("2020/day10/input.txt")
      .getLines()
  }
}
