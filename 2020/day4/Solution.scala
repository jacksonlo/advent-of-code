import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.io.Source
import scala.util.control.NonFatal

object Solution extends App {

  println(partOne(Array("byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"), Array("cid")));
  println(partTwo(Array("byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"), Array("cid")));

  case class Passport(fields: mutable.HashMap[String,String]) {
    def hasRequiredFields(requiredFields: Array[String]): Boolean = {
      requiredFields.forall(field => fields.contains(field))
    }
    def isValid(requiredFields: Array[String]): Boolean = {
      var result = true
      if (!hasRequiredFields(requiredFields)) {
        result &= false
      }

      val byr = fields.getOrElse("byr", "")
      result &= validateRange(byr, 1920, 2002)

      val iyr = fields.getOrElse("iyr", "")
      result &= validateRange(iyr, 2010, 2020)

      val eyr = fields.getOrElse("eyr", "")
      result &= validateRange(eyr, 2020, 2030)

      val HeightCmRE = "([0-9]+)(cm)".r
      val HeightInRE = "([0-9]+)(in)".r
      val hgt = fields.getOrElse("hgt", "")
      hgt match {
        case HeightCmRE(value, unit) => result &= validateRange(value, 150, 193)
        case HeightInRE(value, unit) => result &= validateRange(value, 59, 76)
        case _ => result &= false
      }

      val HclRE = "#([0-9A-Za-z]+){6}".r
      val hcl = fields.getOrElse("hcl", "")
      hcl match {
        case HclRE(colorId) => result &= true
        case _ => result &= false
      }

      val ecl = fields.getOrElse("ecl", "")
      val validEyeColors = Set("amb", "blu", "brn", "gry", "grn", "hzl", "oth")
      result &= validEyeColors.contains(ecl)

      val pid = fields.getOrElse("pid", "")
      val PidRE = "([0-9]+)".r;
      pid match {
        case PidRE(value) => result &= value.length == 9
        case _ => result &= false
      }

      result
    }
  }

  def validateRange(fieldValue: String, min: Int, max: Int) = {
    var result = false
    if (fieldValue != "") {
      result = fieldValue.toInt >= min && fieldValue.toInt <= max
    }
    result
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

  def partTwo(requiredFields: Array[String], optionalFields: Array[String]): Int = {
    val invalidPassports = List(
      Passport(mutable.HashMap("eyr" -> "1972", "cid" -> "100", "hcl" -> "#18171d", "ecl" -> "amb", "hgt" -> "170", "pid" -> "186cm", "iyr" -> "2018", "byr" -> "1926")),
      Passport(mutable.HashMap("iyr" -> "2019", "hcl" -> "#602927", "eyr" -> "1967", "hgt" -> "170cm", "ecl" -> "grn", "pid" -> "012533040", "byr" -> "1946")),
      Passport(mutable.HashMap("hcl" -> "dab227", "iyr" -> "2012", "ecl" -> "brn", "hgt" -> "182cm", "pid" -> "021572410", "eyr" -> "2020", "byr" -> "1992", "cid" -> "277")),
      Passport(mutable.HashMap("hgt" -> "59cm", "ecl" -> "zzz", "eyr" -> "2038", "hcl" -> "74454a", "iyr" -> "2023", "pid" -> "3556412378", "byr" -> "2007")),
    );
    val passports = parsePassports().filter(passport => passport.isValid(requiredFields))
    passports.length
  }
}
