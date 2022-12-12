package rui.aoc.year2022.day12

import rui.aoc.year2022.ProblemSolution
import rui.aoc.year2022.utils.FileIO

import scala.collection.mutable

class Day12Part2 extends ProblemSolution {
  override def solve(): AnyVal = {
    val map = FileIO
      .readResourceLines("day12.txt")
      .map(_.toCharArray)
    val pQueue = mutable.PriorityQueue()(Ordering.by(ClimbingState.climbingStateOrder).reverse)
    val visited: mutable.Set[ClimbingState] = mutable.Set()

    val sPosition = findSymbol(map, 'S')
    val start = findSymbol(map, 'E')
    map(start._2)(start._1) = 'z'
    map(sPosition._2)(sPosition._1) = 'a'
    pQueue.enqueue(ClimbingState(start, 0))

    while (pQueue.nonEmpty) {
      val curr = pQueue.dequeue()
      if (!visited.contains(curr)) {
        visited.addOne(curr)

        if (map(curr.position._2)(curr.position._1) == 'a') {
          return curr.distance
        }

        findNeighbors(map, curr).foreach(neighbor => pQueue.enqueue(
          ClimbingState(neighbor, curr.distance + 1)
        ))
      }
    }

    throw new Exception("Path not found")
  }

  def findNeighbors(map: Array[Array[Char]], state: ClimbingState): Seq[(Int, Int)] = Seq(
    (state.position._1 - 1, state.position._2),
    (state.position._1 + 1, state.position._2),
    (state.position._1, state.position._2 - 1),
    (state.position._1, state.position._2 + 1))
    .filter(_._1 >= 0)
    .filter(_._2 >= 0)
    .filter(_._2 < map.length)
    .filter(_._1 < map.head.length)
    .filter(neighbor =>
      map(state.position._2)(state.position._1) <= map(neighbor._2)(neighbor._1) + 1
    )

  def findSymbol(map: Array[Array[Char]], target: Char): (Int, Int) = {
    for (i <- map.indices; j <- map(i).indices) {
      if (map(i)(j) == target) return (j, i)
    }
    throw new Exception("Destination not found")
  }
}
