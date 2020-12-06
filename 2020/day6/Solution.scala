import java.util

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.io.Source

object Solution {

  def main(args: Array[String]): Unit = {
    println(partOne());
    println(partTwo());
 }

  def partOne(): Int = {
    getGroupAnswers().map(s => s.size).sum
  }

  def partTwo(): Int = {
    getAllAnswered().map(answerSets => {
      answerSets.reduce((total: Set[Char], set: Set[Char]) => total.intersect(set)).size
    }).sum
  }

  private def getGroupAnswers(): List[Set[Char]] = {
    val answers = new ListBuffer[Set[Char]]
    var currentAnswer: String = "";
    parseFile().foreach(line => {
      if (line == "") {
        answers.append(currentAnswer.toSet)
        currentAnswer = ""
      } else {
        currentAnswer += line
      }
    })
    if (currentAnswer != "") {
      answers.append(currentAnswer.toSet)
    }

    answers.toList
  }

  private def getAllAnswered(): List[List[Set[Char]]] = {
    val answers = new ListBuffer[List[Set[Char]]]
    val groupAnswers = new ListBuffer[Set[Char]]
    parseFile().foreach(line => {
      if (line == "") {
        answers.append(groupAnswers.toList)
        groupAnswers.clear()
      } else {
        groupAnswers.append(line.toSet)
      }
    })
    if (groupAnswers.nonEmpty) {
      answers.append(groupAnswers.toList)
    }
    answers.toList
  }

  private def parseFile(): Iterator[String] = {
    Source.fromResource("2020/day6/input.txt")
      .getLines()
  }
}
