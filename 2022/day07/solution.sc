import scala.annotation.tailrec
import scala.io.Source
import scala.util.matching.Regex

final case class File(name: String, size: Long)

final case class Directory(name: String)


object Solution {

  val cdPattern: Regex = "^\\$ cd (.*)$".r
  val lsPattern: Regex = "^\\$ ls$".r
  val filePattern: Regex = "^([0-9]+) (.*)$".r
  val dirPattern: Regex = "^dir (.*)$".r

  def main(args: Array[String]): Unit = {
    println(partOne())
    println(partTwo())
  }

  def partOne(): Long = {
    val data = directoryData
    val directorySizes = data.keys.map(d => getDirectorySize(d, data))
    directorySizes.filter(_ <= 100000).sum
  }

  def partTwo(): Int = {
    1
  }

  private def getDirectorySize(directory: Directory, directoryData: Map[Directory, List[Either[Directory, File]]]): Long = {
    directoryData(directory).map{
      case Left(d) => getDirectorySize(d, directoryData)
      case Right(f) => f.size
    }.sum
  }

  private def directoryData: Map[Directory, List[Either[Directory, File]]] = {
    val lines = input.toList

    // Assume first row is root command: `cd /`
    val root = Directory("/")
    buildFileSystem(lines.drop(1), root, Map())
  }

  @tailrec
  private def buildFileSystem(lines: List[String], currentDirectory: Directory, data: Map[Directory, List[Either[Directory, File]]]): Map[Directory, List[Either[Directory, File]]] = {
    if (lines.isEmpty) {
      return data
    }
    val prefix = if (currentDirectory.name.endsWith("/")) currentDirectory.name else currentDirectory.name + "/"
    lines.head match {
      case cdPattern(target: String) =>
        var newDirectoryName = if (target != "..") prefix + target else prefix.split("/").dropRight(1).mkString("/")
        if (newDirectoryName.isEmpty) {
          newDirectoryName = "/"
        }
        buildFileSystem(lines.drop(1), Directory(newDirectoryName), data)
      case lsPattern() =>
        val fs = parseLS(lines.drop(1), currentDirectory)
        if (data.contains(currentDirectory)) {
          throw new Exception(s"$currentDirectory")
        }
        buildFileSystem(fs._1, currentDirectory, data + (currentDirectory -> fs._2))
    }
  }

  private def parseLS(lines: List[String], currentDirectory: Directory): (List[String], List[Either[Directory, File]]) = {
    val output = lines.takeWhile(x => filePattern.matches(x) || dirPattern.matches(x))
    val prefix = if (currentDirectory.name.endsWith("/")) currentDirectory.name else currentDirectory.name + "/"
    val fileSystem = output.map {
        case dirPattern(name: String) =>
          Left(Directory(prefix + name))
        case filePattern(size: String, name: String) =>
          Right(File(prefix + name, size.toLong))
      }
    (lines.drop(output.length), fileSystem)
  }

  private def input: Iterator[String] = {
    Source
      .fromResource("./input.txt")
      .getLines()
  }

  private def printDirectoryData(dd: Map[Directory, List[Either[Directory, File]]]): Unit = {
    dd.foreach { case (key, values) => println(key.toString + " -> \n\t" + values.mkString("\n\t"))}
  }
}