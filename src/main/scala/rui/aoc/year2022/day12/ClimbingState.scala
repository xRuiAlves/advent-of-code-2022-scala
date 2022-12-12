package rui.aoc.year2022.day12

case class ClimbingState(position: (Int, Int), distance: Int)

object ClimbingState {
  def climbingStateOrder(climbingState: ClimbingState) = climbingState.distance
}
