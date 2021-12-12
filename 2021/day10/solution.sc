import scala.collection.mutable
import scala.io.Source

object Solution {

  def main(args: Array[String]): Unit = {
    println(partOne())
  }

  def partOne(): Int = {
    input.map(line => getFirstIllegalChar(line)).map {
      case Some(')') => 3
      case Some(']') => 57
      case Some('}') => 1197
      case Some('>') => 25137
      case _ => 0
    }.sum
  }

  private def getFirstIllegalChar(line: String): Option[Char] = {
    val stack = mutable.Stack[Char](line.head)
    val openingBrackets = Set('(', '[', '{', '<')
    for(c <- line) {
      if (openingBrackets.contains(c)) {
        stack.push(c)
      } else {
        val isValid = c match {
          case ')' => stack.pop() == '('
          case ']' => stack.pop() == '['
          case '}' => stack.pop() == '{'
          case '>' => stack.pop() == '<'
        }
        if (!isValid) {
          return Some(c)
        }
      }
    }
    None
  }

  private def input: Iterator[String] = {
    Source
      .fromResource("./input.txt")
      .getLines()
  }
}
