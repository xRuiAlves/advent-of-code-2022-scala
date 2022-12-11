package rui.aoc.year2022.day11

import rui.aoc.year2022.ProblemSolution
import rui.aoc.year2022.day10.Day10.computeCycleValues
import rui.aoc.year2022.utils.FileIO

import scala.collection.mutable.ArrayBuffer

class Day11Part2 extends ProblemSolution {

  def applyOp(item: Long, operand: Char, operationDelta: String): Long = (operand, operationDelta) match {
    case ('+', "old") => item + item
    case ('*', "old") => item * item
    case ('+', delta) => item + delta.toInt
    case ('*', delta) => item * delta.toInt
  }

  def processRound(monkeys: Array[Monkey2], divisorsLcm: Long): Unit = {
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

  override def solve(): AnyVal = {
    val monkeys = FileIO
      .readResourceLines("day11.txt")
      .grouped(7)
      .toArray
      .map(parseMonkey)

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

  def parseMonkey(monkeyInput: Array[String]): Monkey2 = {
    val items = monkeyInput(1)
      .substring(18)
      .split(", ")
      .map(_.toLong)
      .to(ArrayBuffer)
    val operand = monkeyInput(2)(23)
    val operationDelta = monkeyInput(2).substring(25)
    val divTestDelta = monkeyInput(3).substring(21).toLong
    val trueTargetMonkey = monkeyInput(4).substring(29).toInt
    val falseTargetMonkey = monkeyInput(5).substring(30).toInt

    Monkey2(items, operand, operationDelta, divTestDelta, trueTargetMonkey, falseTargetMonkey, 0)
  }
}

case class Monkey2(
    items: ArrayBuffer[Long],
    operand: Char,
    operationDelta: String,
    divTestDelta: Long,
    trueTargetMonkey: Int,
    falseTargetMonkey: Int,
    var numItemsInspected: Int
)
