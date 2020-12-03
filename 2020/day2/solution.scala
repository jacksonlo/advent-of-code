import scala.io.Source

object Solution extends App {

  println(partOne());
  println(partTwo());

  case class Policy(min: Int, max: Int, letter: Char, password: String);

  def parsePolicies(): Iterator[Policy] = {
    val policyRE = "([0-9]+)-([0-9]+) ([A-Za-z]): ([A-Za-z]+)".r;

    Source.fromResource("2020/day2/input.txt")
      .getLines()
      .map(line => {
        line match {
          case policyRE(min, max, letter, password) => Policy(min.toInt, max.toInt, letter.charAt(0), password)
          case _ => throw new Exception("no match" + line)
        }
      })
  }

  def partOne(): Int = {
    val answer = parsePolicies().filter((policy: Policy) => {
        val count = policy.password.count(_ == policy.letter);
        count >= policy.min && count <= policy.max
      })

    answer.toList.size
  }

  def partTwo(): Int = {
    val answer = parsePolicies().filter((policy: Policy) => {
      val atFirstPosition: Boolean = policy.password.charAt(policy.min - 1) == policy.letter;
      val atSecondPosition: Boolean = policy.password.charAt(policy.max - 1) == policy.letter;

      (atFirstPosition && !atSecondPosition) || (atSecondPosition && !atFirstPosition)
    })

    answer.toList.size
  }
}
