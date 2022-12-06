package rui.aoc.year2022.day6

object Day6 {
  def countCharsAtMarker(dataStream: String, markerSize: Int): Int = {
    for (i <- 0 until dataStream.length - markerSize - 1) {
      if (isMarker(dataStream.substring(i, i + markerSize), markerSize)) {
        return i + markerSize
      }
    }

    throw new Exception("Solution not found!")
  }

  def isMarker(marker: String, markerSize: Int): Boolean = marker.toSet.size == markerSize
}
