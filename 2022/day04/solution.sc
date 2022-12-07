import scala.io.Source

final case class Assignment(elf1: (Int, Int), elf2: (Int, Int)) {
  def hasFullOverlap: Boolean = {
    val elf2ContainsElf1 = elf1._1 >= elf2._1 && elf1._2 <= elf2._2
    val elf1ContainsElf2 = elf2._1 >= elf1._1 && elf2._2 <= elf1._2
    elf2ContainsElf1 || elf1ContainsElf2
  }

  def hasOverlap: Boolean = {
    val elf2OverlapsElf1 = elf1._1 >= elf2._1 && elf1._1 <= elf2._2
    val elf1OverlapsElf2 = elf2._1 >= elf1._1 && elf2._1 <= elf1._2
    elf2OverlapsElf1 || elf1OverlapsElf2
  }
}

object Solution {

  def main(args: Array[String]): Unit = {
    println(partOne())
    println(partTwo())
  }

  def partOne(): Int = {
    assignments.count(_.hasFullOverlap)
  }

  def partTwo(): Int = {
    assignments.count(_.hasOverlap)
  }

  def assignments: Iterator[Assignment] = {
    input.map(line => {
      val split = line.split(",")
      val first = split(0).split("-").map(_.toInt)
      val second = split(1).split("-").map(_.toInt)
      Assignment((first(0), first(1)), (second(0), second(1)))
    })
  }

  private def input: Iterator[String] = {
    Source
      .fromResource("./input.txt")
      .getLines()
  }
}