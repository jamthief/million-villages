import scala.io.Source
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.Map
import scala.collection.mutable.Set
import scala.util.Random
import java.io.FileNotFoundException

object Main {
  def main(args: Array[String]): Unit = {
    if (args.length != 1) {
      println("Usage: scala Main <parts.txt>")
      System.exit(1)
    }

    val filename = args(0)
    val header = """^\[(.*)\]$""".r
    val blankEntry = """^\(blank\)$""".r
    val data: Map[String, List[String]] = Map()
    var currentList: ListBuffer[String] = ListBuffer()
    var oldHeader: String = ""

    try {
      // Read file
      val source = Source.fromFile(filename)
      for (line <- source.getLines()) {
        line match {
          case header(headerPart) =>
//            println(s"Got header $headerPart")
            if (oldHeader != "") {
              data(oldHeader) = currentList.toList
            }
            oldHeader = headerPart
            currentList = ListBuffer()
          case blankEntry() =>
            currentList.append("")
          case "" =>
//            println(s"Ignoring empty line")
          case entry =>
//            println(s"Adding $entry")
            currentList.append(entry)
        }
      }
    } catch {
      case e: FileNotFoundException =>
        println("oops" + e.toString())
        System.exit(1)
    }

    // last entry
    data(oldHeader) = currentList.toList

    val millionList: ListBuffer[String] = ListBuffer()
    while (millionList.size < 1_000_000) {
      millionList += generateTownName(data)
    }
    println("id,name,latitude,longitude")
    var i = 0
    println(millionList.map(n => {
      i += 1
      new Village(i, n, randomLatitude(), randomLongitude())
    }).mkString("\n"))
  }

  def generateTownName(parts: Map[String, List[String]]) = {
    def randPart(prop: String, bias: Float = 1) = {
      randomEntry(parts.get(prop).get, bias) match {
        case Some(value) => value
        case None => ""
      }
    }
    val prefix = randPart("prefix", (1.0f/6.0f))
    val initial = randPart("initial")
    val vowelGroup = randPart("vowelGroup")
    val ingPart = randPart("ingPart")
    val modifier = randPart("modifier")
    val suffix = randPart("suffix", (2.0f/15.0f))

    s"$prefix$initial$vowelGroup$ingPart$modifier$suffix"
  }

  def randomEntry(list: List[String], bias: Float): Option[String] = {
    if (Random.nextFloat() <= bias) {
      Some(list(Random.nextInt(list.length)))
    } else {
      None
    }
  }

  // within the approximate geographical bounds of England
  def randomLatitude(): Double = {
    49.9 + (Random.nextDouble() * (55.8 - 49.9))
  }

  def randomLongitude(): Double = {
    -5.8 + (Random.nextDouble() * (1.8 - (-5.8)))
  }
}

class Village(id: Int, name: String, latitude: Double, longitude: Double) {
  override def toString: String = s"$id,$name,$latitude,$longitude"
}