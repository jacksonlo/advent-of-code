import scala.io.Source

final case class Position(x: Int, y: Int)

final case class Accumulator(posToE: Map[Position, Int], eToPos: Map[Int, Vector[Position]], flashes: Int) {
  def flash: Accumulator = {
    val newPosToE = posToE
    val newEToPos = eToPos
    val newFlashes = flashes + 
    Accumulator(newPosToE, newEToPos, newFlashes)
  }
}

object Solution {

  def main(args: Array[String]): Unit = {
    println(partOne())

  }

  def partOne(): Int = {
    val init = Accumulator(positionToEnergy, energyToPositions, 0)
    val accumulator = Range.inclusive(1, 100).foldLeft(init)((acc, i) => {
      acc.flash
    })
    accumulator.flashes
  }

  private def positionToEnergy: Map[Position, Int] = {
    input.zipWithIndex.flatMap {
      case (line, y) => line.zipWithIndex.map { case (energy, x) => Position(x, y) -> energy.toInt }
    }.toMap
  }

  private def energyToPositions: Map[Int, Vector[Position]] = {
    val energyPositions: List[(Int, Position)] = input.zipWithIndex.flatMap {
      case (line, y) => line.zipWithIndex.map { case (energy, x) => energy.toInt -> Position(x, y) }
    }.toList
    energyPositions.groupBy(_._1).map { case (energy, keyValues) => energy -> keyValues.map(_._2).toVector}
  }

  private def input: Iterator[String] = {
    Source
      .fromResource("./input.txt")
      .getLines()
  }
}
