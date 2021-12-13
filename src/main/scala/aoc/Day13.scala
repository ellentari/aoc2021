package aoc

import aoc.util.Coordinate

object Day13:

  case class Input(sheet: Sheet, foldInstructions: List[FoldInstruction])

  enum FoldInstruction:
    case FoldHorizontally(y: Int)
    case FoldVertically(x: Int)

  case class Sheet(dots: Set[Coordinate]):
    def height: Int = dots.map(_.y).maxOption.fold(0)(_ + 1)
    def width: Int = dots.map(_.x).maxOption.fold(0)(_ + 1)

    def foldHorizontally(y: Int): Sheet = Sheet(dots.map {
      case c if c.y < y => c
      case c => c.copy(y = y - (c.y - y))
    })

    def foldVertically(x: Int): Sheet = Sheet(dots.map {
      case c if c.x < x => c
      case c => c.copy(x = x - (c.x - x))
    })

    override def toString: String = {
      val arr = Array.fill(height, width)(' ')
      for (dot <- dots)
        arr(dot.y)(dot.x) = '#'
      arr.map(_.mkString).mkString("\n")
    }
  end Sheet

  def solvePart1(input: Input): Int = fold(input.sheet, input.foldInstructions.take(1)).dots.size

  def solvePart2(input: Input): Sheet = fold(input.sheet, input.foldInstructions)

  private def fold(sheet: Sheet, foldInstructions: List[FoldInstruction]): Sheet =
    foldInstructions.foldLeft(sheet) {
      case (s, FoldInstruction.FoldVertically(x)) => s.foldVertically(x)
      case (s, FoldInstruction.FoldHorizontally(x)) => s.foldHorizontally(x)
    }

  private def parseInput(input: List[String]): Input =
    def parseCoordinate(s: String) = s match
      case s"$x,$y" => Coordinate(x.toInt, y.toInt)

    def parseFoldInstruction(s: String) = s match
      case s"fold along y=$y" => FoldInstruction.FoldHorizontally(y.toInt)
      case s"fold along x=$x" => FoldInstruction.FoldVertically(x.toInt)

    val dots = input.takeWhile(_.nonEmpty)
    val instructions = input.dropWhile(_.nonEmpty).tail

    Input(Sheet(dots.map(parseCoordinate).toSet), instructions.map(parseFoldInstruction))
  end parseInput

  @main def solveDay13(): Unit =
    val sampleInput = parseInput(Resources.lines("day13_sample.txt"))
    val input = parseInput(Resources.lines("day13.txt"))

    println(solvePart1(sampleInput)) // 17
    println(solvePart1(input)) // 807
    println(solvePart2(sampleInput)) // O
    println(solvePart2(input)) // LGHEGUEJ
