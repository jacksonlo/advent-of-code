import scala.io.Source

object Solution extends App {

  partOne();

  case class Policy(min: Int, max: Int, letter: Char, password: String);

  def partOne(): Unit = {
    val policyRE = "([0-9]+)-([0-9]+) ([A-Za-z]): ([A-Za-z]+)".r;

    val answer = Source.fromResource("2020/day2/input.txt")
      .getLines()
      .map(line => {
        line match {
          case policyRE(min, max, letter, password) => Policy(min.toInt, max.toInt, letter.charAt(0), password)
          case _ => throw new Exception("no match" + line)
        }
      })
      .filter((policy) => {
        val count = policy.password.count(_ == policy.letter);
        count >= policy.min && count <= policy.max
      })

    println(answer.toList.size)
  }
}
