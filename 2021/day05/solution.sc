import scala.collection.mutable
import scala.io.Source
import scala.math._

final case class Point(x: Int, y: Int)

final case class Line(a: Point, b: Point) {
  def straightPoints: List[Point] = {
    if (a.x == b.x) {
      val small = min(a.y, b.y)
      val big = max(a.y, b.y)
      Range.inclusive(small, big).map(y => Point(a.x, y)).toList
    } else if (a.y == b.y) {
      val small = min(a.x, b.x)
      val big = max(a.x, b.x)
      Range.inclusive(small, big).map(x => Point(x, a.y)).toList
    } else {
      List()
    }
  }

  def allPoints: List[Point] = {
    if (straightPoints.nonEmpty) {
      return straightPoints
    }

    val leftPoint = if (a.x < b.x) a else b
    val rightPoint = if (leftPoint == a) b else a
    val topPoint = if (a.y > b.y) a else b

    val diagonalPoints = if (leftPoint == topPoint) {
      Range.inclusive(leftPoint.x, rightPoint.x).zipWithIndex.map{ case(x, index) => Point(x, leftPoint.y - index)}
    } else {
      Range.inclusive(leftPoint.x, rightPoint.x).zipWithIndex.map{ case(x, index) => Point(x, leftPoint.y + index)}
    }
    diagonalPoints.toList
  }
}

object Solution {

  def main(args: Array[String]): Unit = {
    println(partOne())
    println(partTwo())
  }

  def partOne(): Int = {
    dangerousPoints(line => line.straightPoints)
  }

  def partTwo(): Int = {
    dangerousPoints(line => line.allPoints)
  }

  private def dangerousPoints(pointMethod: Line => List[Point]): Int = {
    val map = lines.foldLeft(mutable.Map[Point, Int]())((map, line) => {
      pointMethod(line).foreach(point => {
        if (!map.contains(point)) {
          map(point) = 0
        }
        map(point) += 1
      })
      map
    })
    map.values.filter(_ > 1).toList.length
  }

  private def lines: Iterator[Line] = {
    input.map(x => {
      val pointSplit = x.split(" -> ")
      val a = pointSplit.head.split(",")
      val b = pointSplit.last.split(",")

      val pointA = Point(a.head.toInt, a.last.toInt)
      val pointB = Point(b.head.toInt, b.last.toInt)
      Line(pointA, pointB)
    })
  }

  private def input: Iterator[String] = {
    Source
      .fromResource("./input.txt")
      .getLines()
  }
}
