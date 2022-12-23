package rui.aoc.year2022.day21

import rui.aoc.year2022.ProblemSolution
import rui.aoc.year2022.utils.FileIO

import scala.collection.mutable

class Day21Part1 extends ProblemSolution {
  private final val ROOT_MONKEY_NAME = "root"

  override def solve(): AnyVal = monkeyYellValue(FileIO
    .readResourceLines("day21.txt")
    .map(Monkey.apply)
    .map(monkey => monkey.name -> monkey)
    .toMap, ROOT_MONKEY_NAME)

  def monkeyYellValue(
    monkeys: Map[String, Monkey],
    monkeyName: String
  ): Long = {
    val monkey = monkeys(monkeyName)
    monkey.value match {
      case Some(value) => value
      case None => {
        val monkeyOp = monkey.monkeyOp.get
        val leftYellValue = monkeyYellValue(monkeys, monkeyOp.leftMonkeyName)
        val rightYellValue = monkeyYellValue(monkeys, monkeyOp.rightMonkeyName)
        monkeyOp.op match {
          case '+' => leftYellValue + rightYellValue
          case '-' => leftYellValue - rightYellValue
          case '*' => leftYellValue * rightYellValue
          case '/' => leftYellValue / rightYellValue
        }
      }
    }
  }
}