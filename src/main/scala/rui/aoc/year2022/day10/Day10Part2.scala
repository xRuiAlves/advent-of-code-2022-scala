package rui.aoc.year2022.day10

import rui.aoc.year2022.ProblemSolution
import rui.aoc.year2022.utils.FileIO

class Day10Part2 extends ProblemSolution {
  override def solve(): AnyVal = {
    val instructions = FileIO.readResourceLines("day10.txt")
    val values = instructions.foldLeft(Vector(1))((values, instruction) => instruction match {
      case "noop" => values.appended(values.last)
      case _ => instruction.split(" ") match {
        case Array(_, delta) => values.appended(values.last).appended(values.last + delta.toInt)
      }
    })

    val ctrDisplayStr = values
      .zipWithIndex
      .foldLeft(Vector.empty[Char]) {
        case (crt, (value, index)) =>
          if ((value - 1 to value + 1).contains(index % 40)) crt.appended('#')
          else crt.appended('.')
      }
      .dropRight(1)
      .grouped(40)
      .map(_.mkString)
      .mkString("\n")
    CrtDisplay(ctrDisplayStr)
  }
}
