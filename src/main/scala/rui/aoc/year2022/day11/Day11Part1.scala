package rui.aoc.year2022.day11

import rui.aoc.year2022.ProblemSolution
import rui.aoc.year2022.day11.Day11.{applyOp, parseMonkeys}
import rui.aoc.year2022.utils.FileIO

class Day11Part1 extends ProblemSolution {
  override def solve(): AnyVal = {
    val monkeys = parseMonkeys(FileIO
      .readResourceLines("day11.txt"))

    (0 until 20).foreach(_ => processRound(monkeys))

    monkeys
      .map(_.numItemsInspected)
      .sorted
      .map(_.toLong)
      .takeRight(2)
      .product
  }

  def processRound(monkeys: Array[Monkey]): Unit = {
    monkeys.foreach(monkey => {
      monkey.items.foreach(item => {
        val valueAfterOp = applyOp(item, monkey.operand, monkey.operationDelta)
        val boredVal = valueAfterOp / 3
        if (boredVal % monkey.divTestDelta == 0) {
          monkeys(monkey.trueTargetMonkey).items.addOne(boredVal)
        } else {
          monkeys(monkey.falseTargetMonkey).items.addOne(boredVal)
        }
        monkey.numItemsInspected += 1
      })
      monkey.items.clear()
    })
  }
}
