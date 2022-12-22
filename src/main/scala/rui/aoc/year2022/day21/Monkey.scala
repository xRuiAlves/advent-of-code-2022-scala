package rui.aoc.year2022.day21

case class Monkey(
  name: String,
  value: Option[Int] = Option.empty,
  monkeyOp: Option[MonkeyOp] = Option.empty
)

object Monkey {
  def apply(rawMonkeyStr: String): Monkey = rawMonkeyStr.split(" ") match {
    case Array(name, value) => Monkey(name.dropRight(1), value = Option(value.toInt))
    case Array(name, leftMonkeyName, op, rightMonkeyName) =>
      Monkey(name.dropRight(1), monkeyOp = Option(MonkeyOp(leftMonkeyName, op.head, rightMonkeyName)))
  }
}
