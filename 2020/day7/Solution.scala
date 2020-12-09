import scala.io.Source

object Solution {

  case class Bag(count: Int, color: String)

  def main(args: Array[String]): Unit = {
    println(partOne("shiny gold"));
    println(partTwo("shiny gold"));
 }

  def partOne(targetColor: String): Int = {
    val adjacencyMap: Map[String, Vector[Bag]] = buildAdjacencyMap()
    var count = 0
    adjacencyMap.foreachEntry({
      (color: String, contains: Vector[Bag]) => {
        if(hasColor(adjacencyMap, targetColor, color)) {
          count += 1
        }
      }
    })
    count
  }

  private def hasColor(adjacencyMap: Map[String, Vector[Bag]], targetColor: String, currentColor: String): Boolean = {
    if (adjacencyMap(currentColor).isEmpty) {
      return false
    } else if (adjacencyMap(currentColor).map(bag => bag.color).contains(targetColor)) {
      return true
    }
    adjacencyMap(currentColor).exists(containedColor => hasColor(adjacencyMap, targetColor, containedColor.color))
  }

  def partTwo(targetColor: String): Int = {
    val adjacencyMap: Map[String, Vector[Bag]] = buildAdjacencyMap()
    adjacencyMap(targetColor)
      .map(bag => bag.count + bag.count * countBags(adjacencyMap, bag.color))
      .sum
  }

  private def countBags(adjacencyMap: Map[String, Vector[Bag]], currentColor: String): Int = {
    if (adjacencyMap(currentColor).isEmpty) {
      return 0
    }

    adjacencyMap(currentColor)
      .map(bag => bag.count + bag.count * countBags(adjacencyMap, bag.color))
      .sum
  }

  private def buildAdjacencyMap(): Map[String, Vector[Bag]] = {
    val BagRE = "([0-9]+) ([A-Za-z][ A-Za-z]*) (bag.*)".r;
    val NoBagsRE = "no other bags[.]".r

    parseFile().map(line => {
      val color = line.slice(0, line.indexOf("bags")).trim
      val contains: Vector[Bag] = line
        .slice(line.indexOf("contain") + 7, line.length)
        .trim
        .split(",")
        .map(containedBagString => {
          containedBagString.trim match {
            case BagRE(count, color, rest) => Bag(count.toInt, color)
            case NoBagsRE() => null
            case _ => throw new Exception("Unable to parse bag: <" + containedBagString + ">")
          }
        })
        .filter(x => x != null)
        .toVector

      color -> contains
    }).toMap
  }

  private def parseFile(): Iterator[String] = {
    Source.fromResource("2020/day7/input.txt")
      .getLines()
  }
}
