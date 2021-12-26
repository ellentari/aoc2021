package aoc

import scala.annotation.tailrec

object Day25:

  case class State(south: Set[(Int, Int)], east: Set[(Int, Int)], height: Int, width: Int):
    val all: Set[(Int, Int)] = south ++ east

    def moveEast: State = copy(
      east = east.map { (i, j) =>
        val (i0, j0) = (i, (j + 1) % width)
        if (all.contains((i0, j0))) (i, j)
        else (i0, j0)
      })

    def moveSouth: State = copy(
      south = south.map { (i, j) =>
        val (i0, j0) = ((i + 1) % height, j)
        if (all.contains((i0, j0))) (i, j)
        else (i0, j0)
      })

  def solvePart1(initial: State): Int =
    @tailrec
    def loop(step: Int, state: State): Int =
      val moved = state.moveEast.moveSouth

      if (moved == state) step
      else loop(step + 1, moved)

    loop(1, initial)

  private def parseInput(input: List[String]): State =
    def collectIndices(char: Char) = input.zipWithIndex.flatMap((row, i) =>
      row.zipWithIndex.collect {
        case (c, j) if c == char => (i, j)
      }).toSet

    State(collectIndices('v'), collectIndices('>'), input.length, input.headOption.fold(0)(_.length))

  @main def solveDay25(): Unit =
    val sampleInput = parseInput(Resources.lines("day25_sample.txt"))
    val input = parseInput(Resources.lines("day25.txt"))

    println(solvePart1(sampleInput)) // 58
    println(solvePart1(input)) // 507
