package rui.aoc.year2022.day25

import rui.aoc.year2022.ProblemSolution
import rui.aoc.year2022.day25.Day25._
import rui.aoc.year2022.utils.FileIO

class Day25Part1 extends ProblemSolution {
  override def solve(): AnyVal = {
    val snafuNumbers = FileIO.readResourceLines("day25.txt")
    val decimalSum = snafuNumbers.map(snafu2decimal).sum
    println(decimal2snafu(decimalSum))
  }
}