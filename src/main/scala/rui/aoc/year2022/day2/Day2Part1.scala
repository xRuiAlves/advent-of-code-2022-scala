package rui.aoc.year2022.day2

import rui.aoc.year2022.ProblemSolution
import rui.aoc.year2022.utils.FileIO

class Day2Part1 extends ProblemSolution {
  case class Round(opponentMove: String, myMove: String)

  override def solve(): AnyVal = {
    val inputLines = FileIO.readResourceLines("day2.txt")
    inputLines
      .map(_.split(" ") match {
        case Array(opponentMove, myMove) => Round(opponentMove, myMove)
      })
      .map(computeScore)
      .sum
  }

  def computeScore(round: Round): Int = {
    val shapeScore = round.myMove match {
      case "X" => 1
      case "Y" => 2
      case "Z" => 3
    }
    val outcomeScore = computeOutcomeScore(round)
    shapeScore + outcomeScore
  }

  def computeOutcomeScore(round: Round): Int = {
    round match {
      case Round("A", "X") => 3
      case Round("A", "Y") => 6
      case Round("A", "Z") => 0
      case Round("B", "X") => 0
      case Round("B", "Y") => 3
      case Round("B", "Z") => 6
      case Round("C", "X") => 6
      case Round("C", "Y") => 0
      case Round("C", "Z") => 3
    }
  }
}
