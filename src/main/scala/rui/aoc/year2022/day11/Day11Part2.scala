package rui.aoc.year2022.day11

import rui.aoc.year2022.ProblemSolution
import rui.aoc.year2022.day10.Day10.computeCycleValues
import rui.aoc.year2022.day11.Day11.{applyOp, parseMonkeys}
import rui.aoc.year2022.utils.FileIO

import scala.collection.mutable.ArrayBuffer

class Day11Part2 extends ProblemSolution {
  override def solve(): AnyVal = {
    val monkeys = parseMonkeys(FileIO
      .readResourceLines("day11.txt"))

    val divisorsLcm = monkeys
      .map(_.divTestDelta)
      .product

    (0 until 10000).foreach(_ => processRound(monkeys, divisorsLcm))

    monkeys
      .map(_.numItemsInspected)
      .sorted
      .map(_.toLong)
      .takeRight(2)
      .product
  }

  def processRound(monkeys: Array[Monkey], divisorsLcm: Int): Unit = {
    monkeys.foreach(monkey => {
      monkey.items.foreach(item => {
        val valueAfterOp = applyOp(item, monkey.operand, monkey.operationDelta)
        val boredVal = valueAfterOp % divisorsLcm
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
