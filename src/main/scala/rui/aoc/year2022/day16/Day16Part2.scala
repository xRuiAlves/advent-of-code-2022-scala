package rui.aoc.year2022.day16

import rui.aoc.year2022.ProblemSolution
import rui.aoc.year2022.day16.Day16.{START_VALVE_NAME, ValveGraph, parseGraph, simplifyGraph}
import rui.aoc.year2022.utils.FileIO

import scala.collection.mutable

class Day16Part2 extends ProblemSolution {
  private final val TIME = 26
  type State = (String, Int, Long)

  override def solve(): AnyVal = {
    val graph = simplifyGraph(parseGraph(FileIO.readResourceLines("day16.txt")))
    graph.values.zipWithIndex.foreach {
      case (valve, i) => valve.id = i
    }
    val initValveId = graph("AA").id
    val swapValve = graph.values.find(_.id == graph.size - 1).get
    graph("AA").id = swapValve.id
    swapValve.id = initValveId

    findBestFlow(graph)
  }

  def visit(graph: ValveGraph): Int = {
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

  def findBestFlow(graph: ValveGraph): Int = {
    val numCombinationValves = graph.size - 1
    val startNodeId = graph.size - 1
    val numIters = math.pow(2, numCombinationValves - 1).toInt
    val valvesById = graph.map {
      case (_, valve) => valve.id -> valve
    }
    var best = 0

    for (bitMask <- 1 until numIters) {
      if (bitMask % 100 == 0) {
        println(bitMask)
      }

      val myGraphValveIds = mutable.Set[Int]()
      val elphGraphValveIds = mutable.Set[Int]()

      for (i <- 0 until numCombinationValves) {
        if ((bitMask & (1 << i)) != 0) {
          myGraphValveIds.addOne(i)
        } else {
          elphGraphValveIds.addOne(i)
        }
      }

      myGraphValveIds.addOne(startNodeId)
      elphGraphValveIds.addOne(startNodeId)

      val myGraph = myGraphValveIds.map(id => {
        val valve = valvesById(id)
        valve.name -> valve.copy(neighbors =
          valve.neighbors.filter(neighbor => myGraphValveIds.contains(graph(neighbor.name).id)))
      }).toMap

      val elphGraph = elphGraphValveIds.map(id => {
        val valve = valvesById(id)
        valve.name -> valve.copy(neighbors =
          valve.neighbors.filter(neighbor => elphGraphValveIds.contains(graph(neighbor.name).id)))
      }).toMap

      best = math.max(best,
        visit(myGraph) + visit(elphGraph)
      )
      println(best)
    }

    best
  }
}
