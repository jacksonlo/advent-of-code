import scala.collection.mutable
import scala.io.Source
import scala.util.matching.Regex

object Solution {

  val OpenBracket = "(";
  val CloseBracket = ")";
  val Multiply = "*";
  val Addition = "+";
  val LongRE: Regex = "([0-9]+)".r;

  def main(args: Array[String]): Unit = {
    println(partOne());
    println(partTwo());
  }

  def partOne(): Long = {
    parseFile()
      .map(str => Equation(str.replace(" ", "")))
      .map(_.evaluate())
      .sum;
  }

  def partTwo(): Long = {
    parseFile()
      .map(_.replace(" ", ""))
      .map(Equation)
      .map(_.evaluate2())
      .sum;
  }

  case class Equation(str: String) {

    def evaluate2(): Long = {
      val AddRE = "([0-9]+)[+]([0-9]+)".r;
      val MultiplyRE = "([0-9]+)[*]([0-9]+)".r;
      val BracketedNumberRE = "([(][0-9]+[)])".r;
      val BracketSectionRE = "[(]([0-9*+]+)[)]".r;
      val SingleNumber = "([0-9]+)";

      var eq = str;
      while(!eq.matches(SingleNumber)) {
        val bracketedSectionMatch: Option[Regex.Match] = BracketSectionRE.findFirstMatchIn(eq);
        if (bracketedSectionMatch.isDefined) {
          val result: Long = Equation(bracketedSectionMatch.get.toString().drop(1).dropRight(1)).evaluate2();
          eq = eq.slice(0, bracketedSectionMatch.get.start) + result + eq.slice(bracketedSectionMatch.get.end, eq.length);
        } else {
          val addMatch: Option[Regex.Match] = AddRE.findFirstMatchIn(eq);
          if (addMatch.isDefined) {
            val result: Long = addMatch.get.subgroups.map(_.toLong).sum;
            eq = eq.slice(0, addMatch.get.start) + result + eq.slice(addMatch.get.end, eq.length);
          } else {
            val bnMatch: Option[Regex.Match] = BracketedNumberRE.findFirstMatchIn(eq);
            if (bnMatch.isDefined) {
              val result: String = bnMatch.get.toString().drop(1).dropRight(1);
              eq = eq.slice(0, bnMatch.get.start) + result + eq.slice(bnMatch.get.end, eq.length);
            } else {
              val multiplyMatch: Option[Regex.Match] = MultiplyRE.findFirstMatchIn(eq);
              if (multiplyMatch.isDefined) {
                val result: Long = multiplyMatch.get.subgroups.map(_.toLong).product;
                eq = eq.slice(0, multiplyMatch.get.start) + result + eq.slice(multiplyMatch.get.end, eq.length);
              }
            }
          }
        }
      }
      eq.toLong;
    }

    def evaluate(): Long = {
      val eq = new mutable.ArrayStack[String];
      str.view.reverse.foreach(c => eq.push(c.toString));

      while(eq.length > 1) {
        val currentTerm = eq.pop();
        if (eq.length > 1) {
          currentTerm match {
            case OpenBracket => {
              val subsection = findSubsection(eq);
              val result = Equation(subsection).evaluate();
              eq.push(result.toString);
            }
            case LongRE(num) => {
              val leftTerm = currentTerm;
              val operator = eq.pop();
              val rightTerm = eq.pop();

              val result = rightTerm match {
                case LongRE(num) => applyOperator(leftTerm, num, operator);
                case OpenBracket => {
                  val subsection = findSubsection(eq);
                  val result = Equation(subsection).evaluate();
                  applyOperator(leftTerm, result.toString, operator);
                };
              }
              eq.push(result.toString);
            }
          }
        }
      }
      eq.head.toLong;
    }

    private def applyOperator(left: String, right: String, operator: String): Long = {
      operator match {
        case Addition => left.toLong + right.toLong;
        case Multiply => left.toLong * right.toLong;
      }
    }

    private def findSubsection(eq: mutable.ArrayStack[String]): String = {
      var subsection = "";
      var current = "";
      var expectedBracket = 1;
      while (expectedBracket != 0) {
        current = eq.pop();
        subsection += current;
        current match {
          case OpenBracket => expectedBracket += 1;
          case CloseBracket => {
            expectedBracket -= 1;
            if (expectedBracket == 0) {
              subsection = subsection.dropRight(1);
            }
          };
          case _ =>
        }
      }
      subsection;
    }
  }

  private def parseFile(): Iterator[String] = {
    Source.fromResource("2020/day18/input.txt")
      .getLines()
  }
}
