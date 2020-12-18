import scala.collection.mutable
import scala.io.Source

object Solution {

  def main(args: Array[String]): Unit = {
    println(partOne());
    println(partTwo());
  }

  def partOne(): Int = {
    val (ticketInfos, yourTicket, nearbyTickets) = parseTickets();
    nearbyTickets.map(ticket => {
      ticket.fields.filter(field => {
        ticketInfos.count(info => isBetween(field, info.range)) == 0;
      }).sum;
    }).sum;
  }

  def partTwo(): Long = {
    val (ticketInfos, yourTicket, nearbyTickets) = parseTickets();
    val validTickets = nearbyTickets.filter(ticket => {
      ticket.fields.count(field => {
        ticketInfos.count(info => isBetween(field, info.range)) > 0;
      }) == ticket.fields.length;
    });

    // Determine columns
    val infoMap = new mutable.HashMap[Int, mutable.HashSet[String]]();
    ticketInfos.foreach(info => {
      for(colIndex <- validTickets.head.fields.indices) {
        val isColMatch = validTickets.forall(ticket => isBetween(ticket.fields(colIndex), info.range));
        if (isColMatch) {
          if (!infoMap.contains(colIndex)) {
            infoMap(colIndex) = new mutable.HashSet[String];
          }
          infoMap(colIndex).add(info.name);
        };
      }
    });

    // Narrow down
    do {
      val narrowed = infoMap.filter(kv => kv._2.size == 1).values.map(_.head).toSet;
      infoMap.foreach((v) => {
        narrowed.foreach(n => {
          if (v._2.size > 1) {
            val m = v._2;
            m.remove(n);
            infoMap(v._1) = m;
          }
        });
      });
    } while(infoMap.exists(kv => kv._2.size > 1));

    val departureIndices = infoMap.filter((kv) => kv._2.head.startsWith("departure")).keys;
    departureIndices.map(i => yourTicket.fields(i).toLong).product;
  }

  private def isBetween(value: Int, ranges: ((Int, Int), (Int, Int))): Boolean = {
    (value >= ranges._1._1 && value <= ranges._1._2) || (value >= ranges._2._1 && value <= ranges._2._2);
  }

  case class TicketInfo(name: String, range: ((Int, Int), (Int, Int)));
  case class Ticket(fields: Vector[Int]);

  private def parseTickets(): (Vector[TicketInfo], Ticket, Vector[Ticket]) = {
    val TicketInfoRE = "([a-z ]+): ([0-9]+)-([0-9]+) or ([0-9]+)-([0-9]+)".r;

    val initialValue = new mutable.ListBuffer[Vector[String]];
    initialValue.append(Vector());
    val sections = parseFile().foldLeft(initialValue) {
      (acc, line) =>
        if (line.trim == "") acc.append(Vector());
        else acc(acc.length - 1) = acc.last :+ line;
        acc
    }.toVector;

    val ticketInfos = sections.head.map {
      case TicketInfoRE(name, a, b, c, d) => TicketInfo(name, ((a.toInt, b.toInt), (c.toInt, d.toInt)));
    }

    val yourTicket = Ticket(sections(1)(1).split(',').map(_.toInt).toVector);

    val nearbyTickets = sections(2).drop(1).map(line => Ticket(line.split(',').map(_.toInt).toVector));

    (ticketInfos, yourTicket, nearbyTickets.toVector);
  }

  private def parseFile(): Iterator[String] = {
    Source.fromResource("2020/day16/input.txt")
      .getLines()
  }
}
