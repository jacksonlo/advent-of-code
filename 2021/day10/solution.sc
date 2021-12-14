import scala.collection.mutable
import scala.io.Source

object Solution {

  def main(args: Array[String]): Unit = {
    println(partOne())
    println(partTwo())
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

  def partTwo(): Long = {
    val autocompleteSequences = input.map(line => getAutocompleteSequence(line)).filterNot(_ == "")
    val lineScores = autocompleteSequences.map(autocompleteSequence => autocompleteSequence.map{
      case ')' => 1
      case ']' => 2
      case '}' => 3
      case '>' => 4
      case _ => 0
    })
    val scores = lineScores.map(scores => scores.foldLeft(0l)((total, score) => total * 5 + score)).toList
    scores.sorted.take(scores.length / 2 + 1).last
  }

  private def getFirstIllegalChar(line: String): Option[Char] = {
    val stack = mutable.Stack[Char]()
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

  private def getAutocompleteSequence(line: String): String = {
    // Iterate until no more chars in line and stack is not empty, then reverse the stack
    val stack = mutable.Stack[Char]()
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
          return ""
        }
      }
    }

    stack.map{
      case '(' => ')'
      case '[' => ']'
      case '{' => '}'
      case '<' => '>'
    }.mkString
  }

  private def input: Iterator[String] = {
    Source
      .fromResource("./input.txt")
      .getLines()
  }
}
