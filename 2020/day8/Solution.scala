import scala.collection.mutable
import scala.io.Source

object Solution {

  case class Bag(count: Int, color: String)

  def main(args: Array[String]): Unit = {
    val instructions = parseFile().toVector;
    println(partOne(instructions));
    println(partTwo(instructions));
 }

  def partOne(instructions: Vector[String]): Tuple2[Int, Boolean] = {
    val InstructionRE = "([a-z]+) ([+]|[-])([0-9]+)".r
    val hitInstructions = new mutable.HashSet[Int]

    var accumulator = 0
    var hasHitInstTwice = false
    var index = 0

    while(!hasHitInstTwice && index < instructions.size) {
      if (!hitInstructions.contains(index)) {
        hitInstructions.+=(index)
        instructions(index) match {
          case InstructionRE(instr, sign, argCount) => {
            instr match {
              case "acc" => {
                sign match {
                  case "+" => accumulator += argCount.toInt
                  case "-" => accumulator -= argCount.toInt
                  case _ => throw new Error("Unexpected accumulator sign: " + sign)
                }
                index += 1
              }
              case "jmp" => {
                sign match {
                  case "+" => index += argCount.toInt
                  case "-" => index -= argCount.toInt
                  case _ => throw new Error("Unexpected jmp sign: " + sign)
                }
              }
              case "nop" => index += 1
            }
          }
          case _ => throw new Error("Unexpected instruction: " + instructions(index))
        }
      } else {
        hasHitInstTwice = true
      }
    }

    Tuple2(accumulator, hasHitInstTwice)
  }

  def partTwo(instructions: Vector[String]): Int = {
    val InstructionRE = "([a-z]+) ([+]|[-])([0-9]+)".r
    for((line,index) <- instructions.view.zipWithIndex) {
      val result = line match {
        case InstructionRE(instr, sign, argCount) => {
          instr match {
            case "jmp" => partOne(instructions.updated(index, line.replace("jmp", "nop")))
            case "nop" => partOne(instructions.updated(index, line.replace("nop", "jmp")))
            case "acc" => Tuple2(0, true)
          }
        }
      }
      if (!result._2) {
        return result._1;
      }
    }
    throw new Error("No completable sequence")
  }

  private def parseFile(): Iterator[String] = {
    Source.fromResource("2020/day8/input.txt")
      .getLines()
  }
}
