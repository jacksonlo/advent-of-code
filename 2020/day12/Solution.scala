import scala.annotation.tailrec
import scala.io.Source

object Solution {

  type Position = (Int, Int);
  val DirectionsCW: Array[Char] = Array('N', 'E', 'S', 'W');
  val DirectionsCCW: Array[Char] = Array('N', 'W', 'S', 'E');
  case class Move(direction: Char, magnitude: Int);

  def main(args: Array[String]): Unit = {
    println(partOne());
    println(partTwo());
  }

  def partOne(): Int = {
    val (x, y) = traverseShipMoves(parseMoves(), (0, 0), 'E');
    Math.abs(x) + Math.abs(y)
  }

  def partTwo(): Int = {
    val shipPos = traverseWayPointMoves(parseMoves(), (10, 1), (0, 0));
    Math.abs(shipPos._1) + Math.abs(shipPos._2);
  }

  @tailrec
  private def traverseShipMoves(moves: Iterator[Move], position: Position, currentDir: Char): Position = {
    if (!moves.hasNext) {
      return position;
    }

    val nextMove: Move = moves.next();
    val mag = nextMove.magnitude;
    val newPosition: Position = nextMove.direction match {
      case 'N' => (position._1, position._2 + mag);
      case 'S' => (position._1, position._2 - mag);
      case 'E' => (position._1 + mag, position._2);
      case 'W' => (position._1 - mag, position._2);
      case 'L' => position;
      case 'R' => position;
      case 'F' =>
        currentDir match {
          case 'N' => (position._1, position._2 + mag);
          case 'S' => (position._1, position._2 - mag);
          case 'E' => (position._1 + mag, position._2);
          case 'W' => (position._1 - mag, position._2);
        }
    }
    val newDir: Char = nextMove.direction match {
      case 'L' => {
        val rotations = DirectionsCCW.indexOf(currentDir) + Math.ceil(mag / 90).toInt;
        DirectionsCCW(rotations % DirectionsCCW.length);
      };
      case 'R' => {
        val rotations = DirectionsCW.indexOf(currentDir) + Math.ceil(mag / 90).toInt;
        DirectionsCW(rotations % DirectionsCW.length);
      };
      case _ => currentDir;
    }
    traverseShipMoves(moves, newPosition, newDir);
  }

  @tailrec
  private def traverseWayPointMoves(moves: Iterator[Move], wayPointPos: Position, shipPos: Position): Position = {
    if (!moves.hasNext) {
      return shipPos;
    }

    val nextMove: Move = moves.next();
    val mag = nextMove.magnitude;
    val (newWayPointPos, newShipPos): (Position, Position) = nextMove.direction match {
      case 'N' => ((wayPointPos._1, wayPointPos._2 + mag), shipPos);
      case 'S' => ((wayPointPos._1, wayPointPos._2 - mag), shipPos);
      case 'E' => ((wayPointPos._1 + mag, wayPointPos._2), shipPos);
      case 'W' => ((wayPointPos._1 - mag, wayPointPos._2), shipPos);
      case 'L' =>
        var rotations = Math.ceil(mag / 90).toInt % DirectionsCCW.length;
        var newWP = wayPointPos;
        while(rotations > 0) {
          newWP = (newWP._2 * -1, newWP._1);
          rotations -= 1;
        }
        (newWP, shipPos);
      case 'R' =>
        var rotations = Math.ceil(mag / 90).toInt % DirectionsCW.length;
        var newWP = wayPointPos;
        while(rotations > 0) {
          newWP = (newWP._2, newWP._1 * -1);
          rotations -= 1;
        }
        (newWP, shipPos);
      case 'F' => (wayPointPos, (shipPos._1 + wayPointPos._1 * mag, shipPos._2 + wayPointPos._2 * mag));
    }
    traverseWayPointMoves(moves, newWayPointPos, newShipPos);
  }

  private def parseMoves(): Iterator[Move] = {
    val MoveRE = "([N|S|E|W|L|R|F])([0-9]+)".r;
    parseFile().map(line => line match {
      case MoveRE(dir, mag) => Move(dir.head, mag.toInt);
    })
  }

  private def parseFile(): Iterator[String] = {
    Source.fromResource("2020/day12/input.txt")
      .getLines()
  }
}
