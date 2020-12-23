import scala.annotation.tailrec
import scala.collection.mutable
import scala.io.Source
import scala.util.matching.Regex

object Solution {

  def main(args: Array[String]): Unit = {
    println(partOne());
    println(partTwo());
  }

  def partOne(): Int = {
    val (rules, messages) = parseRulesMessages();
    messages.map(m => rules.matches(m)).count(x => x);
  }

  def partTwo(): Int = {
    // Longest message is 96 characters
    val (rules, messages) = parseRulesMessages();
    // 0: 8 11                => (8-11)
    // 8: 42 | 42 8           => (42)+
    // 11: 42 31 | 42 11 31   => (42-31) | 42 ((42-31) | (42)(...)(31)) 31

    // Length of 8, 128 items
    val set42: Vector[String] = rules.recurse(42);
    val set31: Vector[String] = rules.recurse(31);

    messages.map(msg => {
      // Starts with 42
      var has42Match = false;
      set42.takeWhile(s => {
        val isMatch = s.r.findFirstMatchIn(msg);
        if (isMatch.isDefined) {
          has42Match = isMatch.get.start == 0;
        }
        !has42Match;
      });

      // Ends with 31
      var endsWith31 = false;
      if (has42Match) {
        set31.takeWhile(s => {
          val isMatch = s.reverse.r.findFirstMatchIn(msg.reverse);
          if (isMatch.isDefined) {
            endsWith31 = isMatch.get.start == 0;
          }
          !endsWith31;
        });
      }

      // After the 42's from the start, 42+ and 31+ in the middle
      var has4231Middle = false;
      if (has42Match && endsWith31) {
        var si = 0;
        var count42 = 0;
        while (set42.contains(msg.slice(si, si + set42.head.length))) {
          si += set42.head.length;
          count42 += 1;
        }

        // Check for 31's
        var count31 = 0;
        if (si > set42.head.length) {
          while(set31.contains(msg.slice(si, si + set31.head.length))) {
            si += set31.head.length;
            count31 += 1;
          }
          if (si == msg.length && count42 > count31 && count42 > 1) {
            has4231Middle = true;
          }
        }
      }

      has42Match && endsWith31 && has4231Middle;
    }).count(x => x);
  }

  case class Rules(ruleMap: Map[Int, String]) {
    val A: String = "\"a\"";
    val B: String = "\"b\"";
    val OneTwoOrRE: Regex = "^([0-9]+) [|] ([0-9]+) ([0-9]+)$".r;
    val SingleOrRE: Regex = "^([0-9]+) [|] ([0-9]+)$".r;
    val DoubleOrRE: Regex = "^([0-9]+) ([0-9]+) [|] ([0-9]+) ([0-9]+)$".r;
    val SubruleRE: Regex = "^([0-9]+) ([0-9]+)$".r;
    val SingleRuleRE: Regex = "^([0-9]+)$".r;

    lazy val normalizedRules: Set[String] = recurse(0).toSet;

    def matches(str: String): Boolean = {
      normalizedRules.contains(str);
    }

    // TODO: Make this tailrec
    def recurse(ruleNumber: Int): Vector[String] = {
      val ruleString = ruleMap(ruleNumber);
      ruleString match {
        case A => Vector("a")
        case B => Vector("b")
        case OneTwoOrRE(first, second, third) => recurse(first.toInt) ++ permutations(recurse(second.toInt), recurse(third.toInt))
        case DoubleOrRE(first, second, third, fourth) =>
          permutations(recurse(first.toInt), recurse(second.toInt)) ++ permutations(recurse(third.toInt), recurse(fourth.toInt))
        case SubruleRE(first, second) => permutations(recurse(first.toInt), recurse(second.toInt));
        case SingleOrRE(leftRule, rightRule) => recurse(leftRule.toInt) ++ recurse(rightRule.toInt);
        case SingleRuleRE(ruleNum) => recurse(ruleNum.toInt);
      }
    }

    def permutations(firstRuleSet: Vector[String], secondRuleSet: Vector[String]): Vector[String] = {
      firstRuleSet.flatMap(s => secondRuleSet.map(s + _));
    }
  };

  private def parseRulesMessages(): (Rules, Vector[String]) = {
    val RuleRE = "([0-9]+): (.+)".r;
    val MessageRE = "([a-b]+)".r;

    val rules = new mutable.HashMap[Int, String]()
    val messages = new mutable.ArrayBuffer[String]();
    parseFile().foreach {
      case RuleRE(ruleNum, ruleString) => rules(ruleNum.toInt) = ruleString.trim;
      case "" =>
      case MessageRE(str) => messages.append(str.trim);
    }
    (Rules(rules.toMap), messages.toVector);
  }

  private def parseFile(): Iterator[String] = {
    Source.fromResource("2020/day19/input.txt")
      .getLines()
  }
}
