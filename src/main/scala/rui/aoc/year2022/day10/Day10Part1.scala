package rui.aoc.year2022.day10

import rui.aoc.year2022.ProblemSolution
import rui.aoc.year2022.utils.FileIO

import scala.collection.mutable

class Day10Part1 extends ProblemSolution {
  override def solve(): AnyVal = {
    val instructions = FileIO.readResourceLines("day10.txt")
    val values = instructions.foldLeft(Vector(1))((values, instruction) => instruction match {
      case "noop" => values.appended(values.last)
      case _ => instruction.split(" ") match {
        case Array(_, delta) => values.appended(values.last).appended(values.last + delta.toInt)
      }
    })

    (20 to  values.length by 40)
      .map(i => i * values(i - 1))
      .sum
  }
}
