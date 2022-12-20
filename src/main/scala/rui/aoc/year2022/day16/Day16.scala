package rui.aoc.year2022.day16

import scala.collection.mutable

object Day16 {
  final val START_VALVE_NAME = "AA"

  def parseGraph(valveLines: Array[String]): ValveGraph = valveLines
    .zipWithIndex
    .map { case (valveStr, i) => Valve.fromStr(valveStr, i) }
    .map { case valve => valve.name -> valve }
    .toMap

  def simplifyGraph(graph: ValveGraph, forceValveOpening: Boolean = false): ValveGraph = {
    val matrix = Array.fill(graph.size) {
      Array.fill(graph.size) { Int.MaxValue / 2 - 1 }
    }
    matrix.indices.foreach(i => matrix(i)(i) = 0)
    graph.values.foreach(valve => valve.neighbors.foreach(neighbor => {
      matrix(valve.id)(graph(neighbor.name).id) = neighbor.distance
    }))

    for (k <- matrix.indices; i <- matrix.indices; j <- matrix.indices) {
      matrix(i)(j) = math.min(matrix(i)(j), matrix(i)(k) + matrix(k)(j))
    }

    val nonEmptyValves = graph.values.filter(_.flowRate > 0)

    matrix.indices.map(id => {
      val valve = graph.values.find(_.id == id).get
      valve.name -> valve.copy(neighbors = nonEmptyValves.filter(_.id != id).map(neighbor =>
        ValveNeighbor(neighbor.name, matrix(id)(neighbor.id) + (if (forceValveOpening) 1 else 0))
      ).to(mutable.Set))
    }).filter {
      case (_, valve) => valve.name.equals("AA") || valve.flowRate > 0
    }.toMap
  }

  type ValveGraph = Map[String, Valve]
}
