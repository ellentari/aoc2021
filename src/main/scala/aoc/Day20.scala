package aoc

import aoc.util.Coordinate

import scala.annotation.tailrec

object Day20:

  enum Pixel:
    case Light, Dark

  case class Grid[A](value: Map[Coordinate, A]):
    def area9x9(center: Coordinate): List[Coordinate] =
      for {
        y <- (-1 to 1).map(center.y + _).toList
        x <- (-1 to 1).map(center.x + _)
      } yield Coordinate(x, y)

  case class Input(imageEnhancementAlgorithm: IndexedSeq[Pixel], image: Grid[Pixel])

  def solvePart1: Input => Int = solve(iterations = 2)

  def solvePart2: Input => Int = solve(iterations = 50)

  def solve(iterations: Int)(input: Input): Int =
    val resultImage = (0 until iterations)
      .foldLeft((input.image, false)) { case ((image, alterOuterPixel), _) =>
        (enhance(image, input.imageEnhancementAlgorithm, alterOuterPixel), !alterOuterPixel)
      }._1

    resultImage.value.count(_._2 == Pixel.Light)

  private def enhance(image: Grid[Pixel], imageEnhancementAlgorithm: IndexedSeq[Pixel], alterOuterPixel: Boolean) =
    Grid(image.value.keySet
      .flatMap(image.area9x9) // expand one layer
      .map { coordinate =>
        val outerPixel = if (!alterOuterPixel) Pixel.Dark else imageEnhancementAlgorithm(0)
        val binary = image
          .area9x9(coordinate)
          .map(image.value.getOrElse(_, outerPixel))
          .map {
            case Pixel.Dark => '0'
            case Pixel.Light => '1'
          }
          .mkString("")
        val key = java.lang.Integer.parseInt(binary, 2)

        coordinate -> imageEnhancementAlgorithm(key)
      }.toMap)

  private def parseInput(lines: List[String]): Input = {
    def mapPixel(char: Char) = char match
      case '#' => Pixel.Light
      case _ => Pixel.Dark

    val enhancementAlgorithm = lines.head.map(mapPixel)
    val image = lines.tail.tail.zipWithIndex
      .flatMap((row, i) => row.zipWithIndex.map((char, j) => Coordinate(j, i) -> mapPixel(char)))

    Input(enhancementAlgorithm, Grid(image.toMap))
  }

  @main def solveDay20(): Unit =
    val sampleInput = parseInput(Resources.lines("day20_sample.txt"))
    val input = parseInput(Resources.lines("day20.txt"))

    println(solvePart1(sampleInput)) // 35
    println(solvePart1(input)) // 5203
    println(solvePart2(sampleInput)) // 3351
    println(solvePart2(input)) // 18806
