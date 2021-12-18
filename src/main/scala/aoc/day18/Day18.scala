package aoc.day18

import aoc.Resources
import cats.syntax.all.*

import scala.annotation.tailrec

object Day18:

  def solvePart1(input: List[SnailFishNumber]): Int = input.reduceLeftOption(_ + _).fold(0)(_.magnitude)

  def solvePart2(input: List[SnailFishNumber]): Int =
    (for {
      (n1, i) <- input.zipWithIndex
      (n2, j) <- input.zipWithIndex if i != j
    } yield (n1 + n2).magnitude)
      .maxOption
      .getOrElse(0)

  @main def solveDay18(): Unit =
    val input = Resources.lines("day18.txt").map(Parser.parseSnailNumber)
    val sampleInput = Resources.lines("day18_sample.txt").map(Parser.parseSnailNumber)

    println(solvePart1(sampleInput)) // 4140
    println(solvePart1(input)) // 3981
    println(solvePart2(sampleInput)) // 3993
    println(solvePart2(input)) // 4687
