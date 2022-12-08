package rui.aoc.year2022.day8

import rui.aoc.year2022.ProblemSolution
import rui.aoc.year2022.day6.Day6.countCharsAtMarker
import rui.aoc.year2022.utils.FileIO

import scala.collection.mutable

class Day8Part1 extends ProblemSolution {

  def findVisibleTrees(trees: mutable.Set[Tree], treeLine: Array[Tree]): Unit = treeLine.foldLeft(-1) {
    case (maxHeight, tree) =>
      if (tree.height > maxHeight) {
        trees.addOne(tree)
        tree.height
      }
      else maxHeight
  }

  override def solve(): AnyVal = {
    val grid: Array[Array[Tree]] = FileIO
      .readResourceLines("day8.txt")
      .zipWithIndex
      .map {
        case (lineStr, y) => lineStr
          .toCharArray
          .map(_ - '0')
          .zipWithIndex
          .map {
            case (height, x) => Tree(height, x, y)
          }
      }
    val trees = mutable.Set[Tree]()

    // Horizontal
    grid.foreach(treeLine => {
      findVisibleTrees(trees, treeLine)
      findVisibleTrees(trees, treeLine.reverse)
    })

    // Vertical
    grid.transpose.foreach(treeLine => {
      findVisibleTrees(trees, treeLine)
      findVisibleTrees(trees, treeLine.reverse)
    })

    trees.size
  }

  case class Tree(height: Int, x: Int, y: Int)
}
