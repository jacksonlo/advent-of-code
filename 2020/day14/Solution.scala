import scala.collection.mutable
import scala.io.Source

object Solution {

  case class WriteCommand(address: Long, value: Long, mask: String) {
    def maskedValue: Long = {
      val maskMap = mask.zipWithIndex.map(_.swap).filter(value => value._2 != 'X').toMap;
      val binaryValue = "0" * (36 - value.toBinaryString.length) + value.toBinaryString;
      val maskedValue = binaryValue.zipWithIndex.foldLeft("")((acc: String, value: (Char, Int)) => {
        val (char, index) = value;
        if (maskMap.contains(index)) acc + maskMap(index) else acc + char;
      });
      BigInt(maskedValue, 2).toLong;
    }

    def maskedAddresses: Vector[Long] = {
      val baseMap = mask.zipWithIndex.map(_.swap);
      val maskMap = baseMap.filter(v => v._2 == '1').toMap;
      val binaryValue = "0" * (36 - address.toBinaryString.length) + address.toBinaryString;
      val baseMaskedAddress = binaryValue.zipWithIndex.foldLeft("")((acc: String, v: (Char, Int)) => {
        val (char, index) = v;
        if (maskMap.contains(index)) acc + maskMap(index) else acc + char
      });

      val xIndices = baseMap.filter(v => v._2 == 'X').map(_._1.toInt).toSet;
      val binaryPerms = binaryPermutations(xIndices.size, "");
      binaryPerms.map((perm: String) => {
        val result = baseMaskedAddress.zipWithIndex.foldLeft(("", 0))((acc: (String, Int), v: (Char, Int)) => {
          var (str, xSoFar) = acc;
          val (char, index) = v;

          if (xIndices.contains(index)) {
            str += perm(xSoFar);
            xSoFar += 1;
          } else {
            str += char;
          }
          (str, xSoFar);
        });
        result;
      }).map(z => BigInt(z._1, 2).toLong).toVector;
    }

    private def binaryPermutations(length: Int, str: String): Vector[String] = {
      if (length == 0) {
        return Vector(str);
      }
      binaryPermutations(length - 1, str + "0") ++ binaryPermutations(length - 1, str + "1");
    }
  };

  def main(args: Array[String]): Unit = {
    println(partOne());
    println(partTwo());
  }

  def partOne(): Long = {
    parseCommands().foldLeft(new mutable.HashMap[Long, Long])((registry: mutable.HashMap[Long, Long], writeCommand: WriteCommand) => {
      registry(writeCommand.address) = writeCommand.maskedValue;
      registry;
    }).values.sum;
  }

  def partTwo(): Long = {
    parseCommands().foldLeft(new mutable.HashMap[Long, Long])((registry: mutable.HashMap[Long, Long], writeCommand: WriteCommand) => {
      writeCommand.maskedAddresses.foreach(address => registry(address) = writeCommand.value);
      registry;
    }).values.sum;
  }

  private def parseCommands(): Iterator[WriteCommand] = {
    val MaskRE = "mask = ([0|1|X]{36})".r;
    val WriteCommandRE = """mem\[([0-9]+)\] = ([0-9]+)""".r;
    val initialValue = ("X" * 36, new mutable.ListBuffer[WriteCommand]);

    val (_, commands) = parseFile().foldLeft(initialValue)((acc: (String, mutable.ListBuffer[WriteCommand]), line: String) => {
      line match {
        case MaskRE(mask) => (mask, acc._2);
        case WriteCommandRE(address, value) => (acc._1, acc._2 :+ WriteCommand(address.toInt, value.toInt, acc._1));
      }
    });
    commands.iterator;
  }

  private def parseFile(): Iterator[String] = {
    Source.fromResource("2020/day14/input.txt")
      .getLines();
  }
}
