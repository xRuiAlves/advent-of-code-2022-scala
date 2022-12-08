package rui.aoc.year2022.day8

import rui.aoc.year2022.ProblemSolution
import rui.aoc.year2022.day6.Day6.countCharsAtMarker
import rui.aoc.year2022.utils.FileIO

import scala.collection.mutable

class Day8Part1 extends ProblemSolution {
  override def solve(): AnyVal = {
    val grid: Array[Array[Int]] = FileIO
      .readResourceLines("day8.txt")
      .map(_
        .toCharArray
        .map(_ - '0')
      )
    val trees = mutable.Set[TreeCoord]()

    // Left to Right
    for (i <- grid.indices) {
      var currMax = -1
      for (j <- grid(i).indices) {
        if (grid(i)(j) > currMax) {
          trees.addOne((i, j))
          currMax = grid(i)(j)
        }
      }
    }


    // Right to Left
    for (i <- grid.indices) {
      var currMax = -1
      for (j <- grid(i).indices.reverse) {
        if (grid(i)(j) > currMax) {
          trees.addOne((i, j))
          currMax = grid(i)(j)
        }
      }
    }

    // Top to Bottom
    for (j <- grid.head.indices) {
      var currMax = -1
      for (i <- grid(j).indices) {
        if (grid(i)(j) > currMax) {
          trees.addOne((i, j))
          currMax = grid(i)(j)
        }
      }
    }

    // Top to Bottom
    for (j <- grid.head.indices) {
      var currMax = -1
      for (i <- grid(j).indices.reverse) {
        if (grid(i)(j) > currMax) {
          trees.addOne((i, j))
          currMax = grid(i)(j)
        }
      }
    }
    trees.size
  }

  type TreeCoord = (Int, Int)
}
