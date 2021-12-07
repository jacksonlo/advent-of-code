import scala.io.Source
import scala.util.Random

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
  }

  def partOne(): Int = {
    val days = Range.inclusive(1, 80)
    days.foldLeft(lanternFishes)((fishes, day) => {
      val newFishes = fishes.map(_.dayPassed()).filter(_.isDefined).map(_.get)
      fishes ++ newFishes
    }).length
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
