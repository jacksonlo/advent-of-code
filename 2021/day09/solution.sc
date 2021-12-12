import scala.collection.mutable
import scala.io.Source

final case class Point(x: Int, y: Int)

object Solution {

  def main(args: Array[String]): Unit = {
    println(partOne())
    println(partTwo())
  }

  def partOne(): Int = {
    val heightMap = inputHeightMap
    val lowPoints = getLowPoints(heightMap)
    lowPoints.map(lowPoint => heightMap(lowPoint.y)(lowPoint.x) + 1).sum
  }

  def partTwo(): Int = {
    val heightMap = inputHeightMap
    val lowPoints = getLowPoints(heightMap)
    val basinSizes = lowPoints.map(lowPoint => getBasinSize(lowPoint, heightMap))
    basinSizes.sorted.takeRight(3).product
  }

  private def getLowPoints(heightMap: Vector[Vector[Int]]): Vector[Point] = {
    val height: Int = heightMap.length
    val width: Int = heightMap.head.length

    val UnreachableMax = 10
    val lowPoints = for {
      y <- Range(0, height)
      x <- Range(0, width)
      current = heightMap(y)(x)
      top = if (y - 1 >= 0) heightMap(y - 1)(x) else UnreachableMax
      bottom = if (y + 1 < height) heightMap(y + 1)(x) else UnreachableMax
      left = if (x - 1 >= 0) heightMap(y)(x - 1) else UnreachableMax
      right = if (x + 1 < width) heightMap(y)(x + 1) else UnreachableMax
      isLowPoint = Vector(top, bottom, left, right).forall(_ > current)
      if isLowPoint
    } yield Point(x, y)
    lowPoints.toVector
  }

  private def getBasinSize(point: Point, heightMap: Vector[Vector[Int]]): Int = {
    val height: Int = heightMap.length
    val width: Int = heightMap.head.length

    val visited = mutable.Set[Point]()
    val stack = mutable.Stack[Point]()
    stack.push(point)
    while(stack.nonEmpty) {
      val current = stack.pop()
      visited.add(current)

      val x = current.x
      val y = current.y

      val top = if (y - 1 >= 0) Some(Point(x, y - 1)) else None
      val bottom = if (y + 1 < height) Some(Point(x, y + 1)) else None
      val left = if (x - 1 >= 0) Some(Point(x - 1, y)) else None
      val right = if (x + 1 < width) Some(Point(x + 1, y)) else None

      val children = Vector(top, bottom, left, right).filter(_.isDefined).map(_.get).filterNot(p => visited.contains(p))
      val basinChildren = children
        .filter(p => heightMap(p.y)(p.x) > heightMap(current.y)(current.x))
        .filter(p => heightMap(p.y)(p.x) != 9)
      stack.addAll(basinChildren)
    }
    visited.size
  }

  private def inputHeightMap: Vector[Vector[Int]] = {
    input.map(_.toCharArray.map(_.asDigit).toVector).toVector
  }

  private def input: Iterator[String] = {
    Source
      .fromResource("./input.txt")
      .getLines()
  }
}
