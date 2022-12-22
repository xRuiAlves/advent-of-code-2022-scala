package rui.aoc.year2022.day21

import rui.aoc.year2022.ProblemSolution
import rui.aoc.year2022.utils.FileIO

import scala.collection.mutable

class Day21Part1 extends ProblemSolution {
  private final val ROOT_MONKEY_NAME = "root"

  override def solve(): AnyVal = {
    val monkeys = FileIO
      .readResourceLines("day21.txt")
      .map(Monkey.apply)
      .map(monkey => monkey.name -> monkey)
      .toMap

    val cache = mutable.Map[String, Long]()
    monkeyYellValue(monkeys, cache, ROOT_MONKEY_NAME)
  }

  def monkeyYellValue(
    monkeys: Map[String, Monkey],
    cache: mutable.Map[String, Long],
    monkeyName: String
  ): Long = cache.get(monkeyName) match {
    case Some(cachedValue) => cachedValue
    case None => {
      val monkey = monkeys(monkeyName)
      cache(monkeyName) = monkey.value match {
        case Some(value) => value
        case None => {
          val monkeyOp = monkey.monkeyOp.get
          val leftYellValue = monkeyYellValue(monkeys, cache, monkeyOp.leftMonkeyName)
          val rightYellValue = monkeyYellValue(monkeys, cache, monkeyOp.rightMonkeyName)
          monkeyOp.op match {
            case '+' => leftYellValue + rightYellValue
            case '-' => leftYellValue - rightYellValue
            case '*' => leftYellValue * rightYellValue
            case '/' => leftYellValue / rightYellValue
          }
        }
      }
      cache(monkeyName)
    }
  }

}