import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.io.Source

object Solution {

  val Active: Char = '#';
  val Inactive: Char = '.';

  case class InfiniteGrid(active: Set[(Int, Int, Int)]) {
    def xRange: Int = {
      val x = active.map(_._1);
      x.max - x.min + 1;
    };
    def yRange: Int = {
      val y = active.map(_._2);
      y.max - y.min + 1;
    };
    def zRange: Int = {
      val z = active.map(_._3);
      z.max - z.min + 1;
    };

    def get(x: Int, y: Int, z: Int): Boolean = {
      active.contains((x, y, z));
    }

    def getActiveNeighbours(x: Int, y: Int, z: Int): Int = {
      var activeNeighbourCount = 0;
      Directions.foreach(d => {
        val neighbour = get(x + d._1, y + d._2, z + d._3);
        if (neighbour) activeNeighbourCount += 1;
      });
      activeNeighbourCount;
    }

    def getActiveCount: Int = {
      var count = 0;
      for(x <- 0 to xRange; y <- 0 to yRange; z <- 0 to zRange) {
        if(get(x, y, z)) {
          count += 1;
        }
      }
      count;
    }

    override def toString: String = {
      var str = "\n<--------- INFINITE CUBE --------->\n";
      for(z <- 0 until zRange) {
        str += s"\nz=${z}\n";
        for(x <- 0 until xRange) {
          for(y <- 0 until yRange) {
            if (get(x, y, z)) {
              str += Active
            } else {
              str += Inactive
            };
          }
          str += "\n";
        }
      }
      str;
    }

  }
  case object InfiniteGrid {
    def fromActiveValues(activeValues: Iterable[(Int, Int, Int)]): InfiniteGrid = {
      val (minX: Int, minY: Int, minZ: Int) = activeValues.foldLeft((Int.MaxValue, Int.MaxValue, Int.MaxValue))((acc: ((Int, Int, Int)), value: (Int, Int, Int)) => {
        (
          scala.math.min(value._1, acc._1),
          scala.math.min(value._2, acc._2),
          scala.math.min(value._3, acc._3),
        )
      });

      // Normalize to starting at 0
      val xNormFactor = 0 - minX;
      val yNormFactor = 0 - minY;
      val zNormFactor = 0 - minZ;

      val activeSet = activeValues.map(v => (v._1 + xNormFactor, v._2 + yNormFactor, v._3 + zNormFactor)).toSet;
      InfiniteGrid(activeSet);
    }
  }

  private val Directions = getDirections.filter(_ != (0, 0, 0));

  // Just gonna duplicate everything...
  case class InfiniteGrid2(active: Set[(Int, Int, Int, Int)]) {
    def xRange: Int = {
      val x = active.map(_._1);
      x.max - x.min + 1;
    };
    def yRange: Int = {
      val y = active.map(_._2);
      y.max - y.min + 1;
    };
    def zRange: Int = {
      val z = active.map(_._3);
      z.max - z.min + 1;
    };
    def wRange: Int = {
      val w = active.map(_._4);
      w.max - w.min + 1;
    }

    def get(x: Int, y: Int, z: Int, w: Int): Boolean = {
      active.contains((x, y, z, w));
    }

    def getActiveNeighbours(x: Int, y: Int, z: Int, w: Int): Int = {
      var activeNeighbourCount = 0;
      Directions2.foreach(d => {
        val neighbour = get(x + d._1, y + d._2, z + d._3, w + d._4);
        if (neighbour) activeNeighbourCount += 1;
      });
      activeNeighbourCount;
    }

    def getActiveCount: Int = {
      var count = 0;
      for(x <- 0 to xRange; y <- 0 to yRange; z <- 0 to zRange; w <- 0 to wRange) {
        if(get(x, y, z, w)) {
          count += 1;
        }
      }
      count;
    }

  }
  case object InfiniteGrid2 {
    def fromActiveValues(activeValues: Iterable[(Int, Int, Int, Int)]): InfiniteGrid2 = {
      val (minX: Int, minY: Int, minZ: Int, minW: Int) = activeValues.foldLeft((Int.MaxValue, Int.MaxValue, Int.MaxValue, Int.MaxValue))((acc: ((Int, Int, Int, Int)), value: (Int, Int, Int, Int)) => {
        (
          scala.math.min(value._1, acc._1),
          scala.math.min(value._2, acc._2),
          scala.math.min(value._3, acc._3),
          scala.math.min(value._3, acc._4),
        )
      });

      // Normalize to starting at 0
      val xNormFactor = 0 - minX;
      val yNormFactor = 0 - minY;
      val zNormFactor = 0 - minZ;
      val wNormFactor = 0 - minW;

      val activeSet = activeValues.map(v => (v._1 + xNormFactor, v._2 + yNormFactor, v._3 + zNormFactor, v._4 + wNormFactor)).toSet;
      InfiniteGrid2(activeSet);
    }
  }

  private val Directions2 = getDirections2.filter(_ != (0, 0, 0, 0));


  def main(args: Array[String]): Unit = {
    println(partOne(6));
    println(partTwo(6));
  }

  def partOne(cycles: Int): Int = {
    var grid: InfiniteGrid = parseGrid();
    var cyclesRemaining = cycles;
    while(cyclesRemaining > 0) {
      val activeSet = new ArrayBuffer[(Int, Int, Int)]();
      for (x <- -1 until grid.xRange + 1; y <- -1 until grid.yRange + 1; z <- -1 until grid.zRange + 1) {
        val cube = grid.get(x, y, z);
        val activeNeighbourCount = grid.getActiveNeighbours(x, y, z);
        if (cube) {
          if (activeNeighbourCount == 2 || activeNeighbourCount == 3) activeSet.append((x, y, z));
        } else {
          if (activeNeighbourCount == 3) activeSet.append((x, y, z));
        }
      }
      grid = InfiniteGrid.fromActiveValues(activeSet);
      cyclesRemaining -= 1;
    }
    grid.getActiveCount;
  }

  def partTwo(cycles: Int): Int = {
    var grid: InfiniteGrid2 = parseGrid2();
    var cyclesRemaining = cycles;
    while(cyclesRemaining > 0) {
      val activeSet = new ArrayBuffer[(Int, Int, Int, Int)]();
      for (x <- -1 until grid.xRange + 1; y <- -1 until grid.yRange + 1; z <- -1 until grid.zRange + 1; w <- -1 until grid.wRange + 1) {
        val cube = grid.get(x, y, z, w);
        val activeNeighbourCount = grid.getActiveNeighbours(x, y, z, w);
        if (cube) {
          if (activeNeighbourCount == 2 || activeNeighbourCount == 3) activeSet.append((x, y, z, w));
        } else {
          if (activeNeighbourCount == 3) activeSet.append((x, y, z, w));
        }
      }
      grid = InfiniteGrid2.fromActiveValues(activeSet);
      cyclesRemaining -= 1;
    }
    grid.getActiveCount;
  }

  private def parseGrid(): InfiniteGrid = {
    // Input is at z=0
    val active = new mutable.ArrayBuffer[(Int, Int, Int)]();
    parseFile().zipWithIndex.foreach(v => {
      val (line, x) = v;
      line.toCharArray.zipWithIndex.foreach(v2 => {
        val (char, y) = v2;
        if (char == Active) active.append((x, y, 0));
      });
    });
    InfiniteGrid.fromActiveValues(active);
  }

  private def parseGrid2(): InfiniteGrid2 = {
    // Input is at z=0, w=0
    val active = new mutable.ArrayBuffer[(Int, Int, Int, Int)]();
    parseFile().zipWithIndex.foreach(v => {
      val (line, x) = v;
      line.toCharArray.zipWithIndex.foreach(v2 => {
        val (char, y) = v2;
        if (char == Active) active.append((x, y, 0, 0));
      });
    });
    InfiniteGrid2.fromActiveValues(active);
  }

  private def getDirections: Vector[(Int, Int, Int)] = {
    def permutations(tuple: Vector[Int]): Vector[Vector[Int]] = {
      if (tuple.length == 3) {
        return Vector(tuple);
      }
      permutations(tuple :+ -1) ++ permutations(tuple :+ 0) ++ permutations(tuple :+ 1);
    }

    permutations(Vector()).map(x => (x(0), x(1), x(2)));
  }

  private def getDirections2: Vector[(Int, Int, Int, Int)] = {
    def permutations(tuple: Vector[Int]): Vector[Vector[Int]] = {
      if (tuple.length == 4) {
        return Vector(tuple);
      }
      permutations(tuple :+ -1) ++ permutations(tuple :+ 0) ++ permutations(tuple :+ 1);
    }

    permutations(Vector()).map(x => (x(0), x(1), x(2), x(3)));
  }

  private def parseFile(): Iterator[String] = {
    Source.fromResource("2020/day17/input.txt")
      .getLines()
  }
}
