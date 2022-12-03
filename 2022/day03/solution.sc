import scala.io.Source

final case class RucksackGroup(c1: String, c2: String, c3: String) {
  def priority(): Int = {
    val commonItem = c1.toSet.intersect(c2.toSet).intersect(c3.toSet).last
    val score = commonItem.toUpper.toInt - 64
    if (commonItem.isUpper) score + 26 else score
  }
}

final case class Rucksack(c1: String, c2: String) {
  def priority(): Int = {
    val commonItem: Char = c1.toSet.intersect(c2.toSet).toList.last
    val score = commonItem.toUpper.toInt - 64
    if (commonItem.isUpper) score + 26 else score
  }
}

object Solution {

  def main(args: Array[String]): Unit = {
    println(partOne())
    println(partTwo())
  }

  def partOne(): Int = {
    rucksacks.map(r => r.priority()).sum
  }

  def partTwo(): Int = {
    rucksackGroups.map(rg => rg.priority()).sum
  }

  private def rucksacks: Iterator[Rucksack] = {
    input.map(x => Rucksack(x.substring(0, x.length / 2), x.substring(x.length / 2)))
  }

  private def rucksackGroups: Iterator[RucksackGroup] = {
    input.grouped(3).map(group => RucksackGroup(group(0), group(1), group(2)))
  }

  private def input: Iterator[String] = {
    Source
      .fromResource("./input.txt")
      .getLines()
  }
}
