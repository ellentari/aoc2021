package aoc

object Day2:

  case class State(x: Int = 0, depth: Int = 0, aim: Int = 0):
    def incX(value: Int): State = copy(x = x + value)
    def incAim(value: Int): State = copy(aim = aim + value)
    def incDepth(value: Int): State = copy(depth = depth + value)

  enum Command:
    case Forward(value: Int)
    case Up(value: Int)
    case Down(value: Int)

  import Command.*

  val solvePart1: List[Command] => Int = solve {
    case (state, Forward(x)) => state.incX(x)
    case (state, Up(x)) => state.incDepth(-x)
    case (state, Down(x)) => state.incDepth(x)
  }

  val solvePart2: List[Command] => Int = solve {
    case (state, Forward(x)) => state.incX(x).incDepth(state.aim * x)
    case (state, Up(x)) => state.incAim(-x)
    case (state, Down(x)) => state.incAim(x)
  }

  private def solve(nextState: (State, Command) => State)(input: List[Command]) =
    val result = input.foldLeft(State())(nextState)
    result.x * result.depth

  private def parseCommand(s: String): Command = s match
    case s"forward $x" => Forward(x.toInt)
    case s"up $x" => Up(x.toInt)
    case s"down $x" => Down(x.toInt)

  @main def solveDay2(): Unit =
    val input = Resources.lines("day2.txt").map(parseCommand)
    println(solvePart1(input)) // 1654760
    println(solvePart2(input)) // 1956047400
