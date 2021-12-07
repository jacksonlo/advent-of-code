import scala.io.Source

final case class LanternFish(initialTimer: Int) {
  var timer: Int = initialTimer
  var cycles: Int = if (initialTimer == 8) 0 else 1

  def dayPassed(): Option[LanternFish] = {
    if (timer == 0) {
      timer = 7
      cycles += 1
    }
    timer -= 1
    if (cycles > 0 && timer == 6) {
      Option(LanternFish(8))
    } else {
      None
    }
  }
}

object Solution {

  def main(args: Array[String]): Unit = {
    println(partOne())
    println(partTwo())
  }

  def partOne(): Int = {
    val days = Range.inclusive(1, 80)
    days.foldLeft(lanternFishes)((fishes, day) => {
      val newFishes = fishes.map(_.dayPassed()).filter(_.isDefined).map(_.get)
      fishes ++ newFishes
    }).length
  }

  def partTwo(): Long = {
    val days = Range.inclusive(1, 256)
    val initialFishMap: Map[Int, Long] = lanternFishes.groupBy(_.timer).map{case (key, value) => (key, value.length)}
    val finalFishMap = days.foldLeft(initialFishMap)((fishMap, day) => {
      val fishValues = fishMap.map{case (timer, fishCount) => {
        timer match {
          case 0 => List((6, fishCount), (8, fishCount))
          case _ => List((timer - 1, fishCount))
        }
      }}
      fishValues.flatten.groupBy(_._1).map{case (key, value) => (key, value.map(_._2).sum)}
    })
    finalFishMap.values.sum
  }

  private def lanternFishes: List[LanternFish] = {
    input.toList.head.split(',').map(_.toInt).map(value => LanternFish(value)).toList
  }

  private def input: Iterator[String] = {
    Source
      .fromResource("./input.txt")
      .getLines()
  }
}
