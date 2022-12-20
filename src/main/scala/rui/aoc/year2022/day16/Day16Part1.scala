package rui.aoc.year2022.day16

import rui.aoc.year2022.ProblemSolution
import rui.aoc.year2022.day16.Day16.{START_VALVE_NAME, ValveGraph, parseGraph, simplifyGraph}
import rui.aoc.year2022.utils.FileIO

import scala.collection.mutable

class Day16Part1 extends ProblemSolution {
  private final val TIME = 30
  type State = (String, Int, Long)

  override def solve(): AnyVal = {
    val graph = simplifyGraph(parseGraph(FileIO.readResourceLines("day16.txt")))
    val cache = mutable.Map[State, Int]()
    visit(graph, cache, START_VALVE_NAME, TIME, 0L)
  }

  def computeFlowProduction(graph: ValveGraph, bitMap: Long): Int = graph.values.map {
    case valve => if (isValveTurnedOn(valve, bitMap)) {
      valve.flowRate
    } else 0
  }.sum

  def visit(
    graph: ValveGraph,
    cache: mutable.Map[State, Int],
    currValveName: String,
    timeLeft: Int,
    turnedOnValvesBitMap: Long
  ): Int = if (timeLeft == 0) 0 else cache.get(currValveName, timeLeft, turnedOnValvesBitMap) match {
    case Some(cachedVal) => cachedVal
    case None =>
      val valve = graph(currValveName)
      val flowProduction = computeFlowProduction(graph, turnedOnValvesBitMap)

      val validNeighbors = valve.neighbors.filter(neighbor => timeLeft - neighbor.distance >= 0)
      val visitingBestNeighbor =
        if (validNeighbors.isEmpty) 0
        else validNeighbors.map(neighbor => neighbor.distance * flowProduction +
          visit(graph, cache, neighbor.name, timeLeft - neighbor.distance, turnedOnValvesBitMap)).max

      cache((currValveName, timeLeft, turnedOnValvesBitMap)) = math.max(
        if (valve.flowRate == 0 || isValveTurnedOn(valve, turnedOnValvesBitMap)) visitingBestNeighbor
        else {
          val updatedBipMap = turnedOnValvesBitMap | (1L << valve.id)
          val visitingSelf = flowProduction + visit(graph, cache, currValveName, timeLeft - 1, updatedBipMap)
          math.max(visitingSelf, visitingBestNeighbor)
        },
        flowProduction * timeLeft
      )
      cache((currValveName, timeLeft, turnedOnValvesBitMap))
  }

  def isValveTurnedOn(valve: Valve, turnedOnValvesBitMap: Long): Boolean =
    (turnedOnValvesBitMap & (1L << valve.id)) != 0
}
