package aoc

import scala.annotation.tailrec

object Day10:

  case class Line(brackets: List[Bracket])

  sealed trait Bracket extends Product with Serializable
  object Bracket:
    sealed trait Open extends Bracket
    sealed trait Close extends Bracket

    case object `(` extends Open
    case object `)` extends Close

    case object `[` extends Open
    case object `]` extends Close

    case object `{` extends Open
    case object `}` extends Close

    case object `<` extends Open
    case object `>` extends Close
  end Bracket

  enum SyntaxError:
    case UnexpectedClosingBracket(char: Bracket.Close)
    case Incomplete(expected: List[Bracket.Close])

  import Bracket.*
  import SyntaxError.*

  def solvePart1(input: List[Line]): Int =
    input
      .flatMap(getSyntaxError)
      .collect { case UnexpectedClosingBracket(char) => char }
      .map(part1Points)
      .sum

  def solvePart2(input: List[Line]): Long =
    val sortedResult = input
      .flatMap(getSyntaxError)
      .collect { case Incomplete(expected) => expected }
      .map(_.map(part2Points).foldLeft(0L)(_ * 5 + _))
      .sorted

    sortedResult(sortedResult.length / 2)
  end solvePart2

  private def getSyntaxError(line: Line): Option[SyntaxError] =
    def closingBracket(open: Bracket.Open): Bracket.Close = open match
      case `(` => `)`
      case `[` => `]`
      case `{` => `}`
      case `<` => `>`

    @tailrec
    def loop(brackets: List[Bracket], stack: List[Bracket.Open]): Option[SyntaxError] = {
      brackets match
        case Nil => Option.unless(stack.isEmpty)(Incomplete(stack.map(closingBracket)))
        case (b: Bracket.Open) :: tail => loop(tail, b :: stack)
        case (b: Bracket.Close) :: tail =>
          stack match
            case stackTop :: stackTail if closingBracket(stackTop) == b => loop(tail, stackTail)
            case _ => Some(UnexpectedClosingBracket(b))
    }

    loop(line.brackets, Nil)
  end getSyntaxError

  private def part1Points(bracket: Bracket.Close): Int = bracket match
    case `)` => 3
    case `]` => 57
    case `}` => 1197
    case `>` => 25137

  private def part2Points(bracket: Bracket.Close): Int = bracket match
    case `)` => 1
    case `]` => 2
    case `}` => 3
    case `>` => 4

  private def parseLine(raw: String): Line = Line(raw.toList.map {
    case '(' => `(`
    case ')' => `)`
    case '[' => `[`
    case ']' => `]`
    case '{' => `{`
    case '}' => `}`
    case '<' => `<`
    case '>' => `>`
  })

  @main def solveDay10(): Unit =
    val sampleInput = Resources.lines("day10_sample.txt").map(parseLine)
    val input = Resources.lines("day10.txt").map(parseLine)

    println(solvePart1(sampleInput)) //  26397
    println(solvePart1(input)) //  389589
    println(solvePart2(sampleInput)) // 288957
    println(solvePart2(input)) // 1190420163
