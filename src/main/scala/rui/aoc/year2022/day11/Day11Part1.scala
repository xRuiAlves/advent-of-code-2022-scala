package rui.aoc.year2022.day11

import rui.aoc.year2022.ProblemSolution
import rui.aoc.year2022.day10.Day10.computeCycleValues
import rui.aoc.year2022.utils.FileIO

import scala.collection.mutable.ArrayBuffer

class Day11Part1 extends ProblemSolution {
  override def solve(): AnyVal = {
    val monkeys = FileIO
      .readResourceLines("day11.txt")
      .grouped(7)
      .toArray
      .map(parseMonkey)

    (0 until 20).foreach(_ => processRound(monkeys))
    monkeys
      .map(_.numItemsInspected)
      .sorted
      .takeRight(2)
      .product
  }

  def parseMonkey(monkeyInput: Array[String]): Monkey = {
    val items = monkeyInput(1)
      .substring(18)
      .split(", ")
      .map(_.toLong)
      .to(ArrayBuffer)
    val operand = monkeyInput(2)(23)
    val operationDelta = monkeyInput(2).substring(25)
    val divTestDelta = monkeyInput(3).substring(21).toInt
    val trueTargetMonkey = monkeyInput(4).substring(29).toInt
    val falseTargetMonkey = monkeyInput(5).substring(30).toInt

    Monkey(items, operand, operationDelta, divTestDelta, trueTargetMonkey, falseTargetMonkey, 0)
  }

  def applyOp(item: Long, operand: Char, operationDelta: String): Long = (operand, operationDelta) match {
    case ('+', "old") => item + item
    case ('*', "old") => item * item
    case ('+', delta) => item + delta.toInt
    case ('*', delta) => item * delta.toInt
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
