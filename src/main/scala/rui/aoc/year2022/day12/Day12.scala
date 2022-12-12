package rui.aoc.year2022.day12

import scala.collection.mutable

object Day12 {
  def parseMap(rawInput: Array[String]): Map = rawInput.map(_.toCharArray)

  def findSymbol(map: Map, target: Char): Coordinate = {
    for (i <- map.indices; j <- map(i).indices) {
      if (map(i)(j) == target) return (j, i)
    }
    throw new Exception("Destination not found")
  }

  def findNeighbors(
    map: Map,
    state: ClimbingState,
    isNeighborValid: (Map, Coordinate, Coordinate) => Boolean
  ): Seq[Coordinate] = Seq(
    (state.position._1 - 1, state.position._2),
    (state.position._1 + 1, state.position._2),
    (state.position._1, state.position._2 - 1),
    (state.position._1, state.position._2 + 1))
    .filter(_._1 >= 0)
    .filter(_._2 >= 0)
    .filter(_._2 < map.length)
    .filter(_._1 < map.head.length)
    .filter(neighbor => isNeighborValid(map, state.position, neighbor)
  )

  def findMinPath(
    start: Coordinate,
    map: Map,
    isDestination: (Map, Coordinate) => Boolean,
    isNeighborValid: (Map, Coordinate, Coordinate) => Boolean
  ): Int = {
    val pQueue = mutable.PriorityQueue()(Ordering.by(ClimbingState.climbingStateOrder).reverse)
    val visited: mutable.Set[ClimbingState] = mutable.Set()
    pQueue.enqueue(ClimbingState(start, 0))

    while (pQueue.nonEmpty) {
      val curr = pQueue.dequeue()

      if (!visited.contains(curr)) {
        visited.addOne(curr)

        if (isDestination(map, curr.position)) {
          return curr.distance
        }

        findNeighbors(map, curr, isNeighborValid).foreach(neighbor => pQueue.enqueue(
          ClimbingState(neighbor, curr.distance + 1)
        ))
      }
    }

    throw new Exception("Path not found")
  }

  type Coordinate = (Int, Int)
  type Map = Array[Array[Char]]

  final val START = 'S'
  final val DESTINATION = 'E'
  final val MIN_HEIGHT = 'a'
  final val MAX_HEIGHT = 'z'
}
