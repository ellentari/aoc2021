package aoc

object Day2 extends App {

  case class State(x: Int = 0, depth: Int = 0) {
    def incX(value: Int): State = copy(x = x + value)
    def incDepth(value: Int): State = copy(depth = depth + value)
  }

  case class StatePt2(state: State = State(), aim: Int = 0) {
    def incAim(value: Int): StatePt2 = copy(aim = aim + value)
    def forward(value: Int): StatePt2 = copy(state = state.incX(value).incDepth(aim * value))
  }

  sealed trait Command
  object Command {
    case class Forward(value: Int) extends Command
    case class Up(value: Int) extends Command
    case class Down(value: Int) extends Command
  }

  import Command._

  val solvePart1: List[Command] => Int = solve(State())(identity) {
    case (state, Forward(x)) => state.incX(x)
    case (state, Up(x)) => state.incDepth(-x)
    case (state, Down(x)) => state.incDepth(x)
  }

  val solvePart2: List[Command] => Int = solve(StatePt2())(_.state) {
    case (state, Forward(x)) => state.forward(x)
    case (state, Up(x)) => state.incAim(-x)
    case (state, Down(x)) => state.incAim(x)
  }

  private def solve[S](initialState: S)(finalResult: S => State)(nextState: (S, Command) => S)(input: List[Command]) = {
    val result = finalResult(input.foldLeft(initialState)(nextState))

    result.x * result.depth
  }

  private def parseCommand(s: String): Command = s match {
    case s"forward $x" => Forward(x.toInt)
    case s"up $x" => Up(x.toInt)
    case s"down $x" => Down(x.toInt)
  }

  private val input = Resources.lines("day2.txt").map(parseCommand)

  println(solvePart1(input)) // 1654760
  println(solvePart2(input)) // 1956047400

}
