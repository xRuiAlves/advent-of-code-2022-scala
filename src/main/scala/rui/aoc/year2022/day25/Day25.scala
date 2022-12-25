package rui.aoc.year2022.day25

object Day25 {
  private final val SNAFU_BASE_SIZE = 5

  def snafu2decimal(snafuNumber: String): Long = snafuNumber
    .reverse
    .zipWithIndex
    .foldLeft(0L) {
      case (acc, (digit, index)) => acc + Math.pow(SNAFU_BASE_SIZE, index).toLong * (digit match {
        case '=' => -2L
        case '-' => -1L
        case _ => (digit - '0').toLong
      })
    }

  def decimal2snafu(decimalNumber: Long): String = if (decimalNumber == 0) "" else decimalNumber % SNAFU_BASE_SIZE match {
    case 0 => s"${decimal2snafu(decimalNumber / SNAFU_BASE_SIZE)}0"
    case 1 => s"${decimal2snafu((decimalNumber - 1) / SNAFU_BASE_SIZE)}1"
    case 2 => s"${decimal2snafu((decimalNumber - 2) / SNAFU_BASE_SIZE)}2"
    case 3 => s"${decimal2snafu((decimalNumber + 2) / SNAFU_BASE_SIZE)}="
    case 4 => s"${decimal2snafu((decimalNumber + 1) / SNAFU_BASE_SIZE)}-"
  }
}
