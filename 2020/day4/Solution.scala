import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.io.Source
import scala.util.control.NonFatal

object Solution extends App {

  println(partOne(Array("byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"), Array("cid")));

  case class Passport(fields: mutable.HashMap[String,String]) {
    def hasRequiredFields(requiredFields: Array[String]): Boolean = {
      requiredFields.forall(field => fields.contains(field))
    }
  }

  def parsePassports(): List[Passport] = {
    val passports = new ListBuffer[Passport]();
    var currentPassportLine: String = "";
    Source.fromResource("2020/day4/input.txt")
      .getLines()
      .foreach((line: String) => {
        if (line.trim == "") {
          val passport = Passport(parseLine(currentPassportLine));
          passports.append(passport);
          currentPassportLine = "";
        } else {
          currentPassportLine = currentPassportLine + " " + line;
        }
      })
      if (currentPassportLine != "") {
        passports.append(Passport(parseLine(currentPassportLine)))
      }
      passports.toList
  }

  def parseLine(line: String): mutable.HashMap[String, String] = {
    val fields = new mutable.HashMap[String,String]();
    line.split(" ").foreach((fieldString: String) => {
      if (fieldString.trim != "") {
        val values = fieldString.split(":");
        if (values.length != 2) {
          throw new Error("Unexpected passport field: " + fieldString + " - " + line)
        }
        val fieldName = values(0).trim
        val fieldValue = values(1).trim
        fields(fieldName) = fieldValue;
      }
    })
    fields
  }

  def partOne(requiredFields: Array[String], optionalFields: Array[String]): Int = {
    val testPassports = List(
      Passport(mutable.HashMap("ecl" -> "gry", "pid" -> "860033327", "eyr" -> "2020", "hcl" -> ":#fffffd", "byr" -> "1937", "iyr" -> "2017", "cid" -> "147", "hgt" -> "183cm")),
      Passport(mutable.HashMap("iyr" -> "2013", "ecl" -> "amb", "cid" -> "350", "eyr" -> "2023", "pid" -> "028048884", "hcl" -> ":#cfa07d", "byr" -> "1929")),
      Passport(mutable.HashMap("hcl" -> "#ae17e1", "iyr" -> "2013", "eyr" -> "2024", "ecl" -> "brn", "pid" -> "760753108", "byr" -> "1931", "hgt" -> "179cm")),
      Passport(mutable.HashMap("hcl" -> "#cfa07d", "eyr" -> "2025", "pid" -> "166559648", "iyr" -> "2011", "ecl" -> "brn", "hgt" -> "59in")),
    );
    val passports = parsePassports().filter(passport => {
      passport.hasRequiredFields(requiredFields)
    })
    passports.length
  }

}
