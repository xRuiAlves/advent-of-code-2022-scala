package rui.aoc.year2022.day8

import rui.aoc.year2022.ProblemSolution
import rui.aoc.year2022.utils.FileIO

import scala.collection.mutable

class Day8Part2 extends ProblemSolution {

  def getScore(grid: Array[Array[Int]], i: Int, j: Int): Int = {
    val me = grid(i)(j)

    var r = 0
    var l = 0
    var t = 0
    var b = 0

    def countRight(): Unit = {
      for (z <- j + 1 until grid(i).length) {
        r += 1
        if (grid(i)(z) >= me) return
      }
    }

    def countLeft(): Unit = {
      for (z <- j - 1  to 0 by -1) {
        l += 1
        if (grid(i)(z) >= me) return
      }
    }

    def countTop(): Unit = {
      for (z <- i - 1 to 0 by -1) {
        t += 1
        if (grid(z)(j) >= me) return
      }
    }

    def countBottom(): Unit = {
      for (z <- i + 1 until  grid.length) {
        b += 1
        if (grid(z)(j) >= me) return
      }
    }

    countLeft()
    countRight()
    countTop()
    countBottom()

    println(r * l * t * b)
    r * l * t * b
  }

  override def solve(): AnyVal = {
    val grid: Array[Array[Int]] = FileIO
      .readResourceLines("day8.txt")
      .map(_
        .toCharArray
        .map(_ - '0')
      )

    var max: Int = 0
    for (i <- grid.indices) {
      for (j <- grid(i).indices) {
        max = math.max(max, getScore(grid, i, j))
      }
    }
    max
  }

  type TreeCoord = (Int, Int)
}
