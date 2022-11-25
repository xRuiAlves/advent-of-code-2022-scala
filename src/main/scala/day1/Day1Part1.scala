package day1

import utils.FileIO

object Day1Part1 {
  def main(args: Array[String]): Unit = {
    val inputLines = FileIO.readResourceLines("day1.txt")

    var numMeasurements = 0
    for (i <- 1 until inputLines.size) {
      numMeasurements += (
        if (inputLines(i) > inputLines(i - 1)) 1
        else 0
      )
    }

    println(numMeasurements)
  }
}
