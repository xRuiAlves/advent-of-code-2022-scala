package utils

import scala.io.Source
import scala.util.Using

object FileIO {
  def readResourceLines(resourceName: String): List[String] = {
    Using(Source.fromResource(resourceName)) { _.getLines().toList }.get
  }
}
