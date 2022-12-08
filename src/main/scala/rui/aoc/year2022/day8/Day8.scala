package rui.aoc.year2022.day8

object Day8 {
  def parseTreeGrid(input: Array[String]): Array[Array[Tree]] = {
    input.zipWithIndex
      .map {
        case (lineStr, y) => lineStr
          .toCharArray
          .map(_ - '0')
          .zipWithIndex
          .map {
            case (height, x) => Tree(height, x, y)
          }
      }
  }
}
