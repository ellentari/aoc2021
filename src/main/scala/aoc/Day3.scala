package aoc

import scala.annotation.tailrec

object Day3:

  type CharCriteria = Map[Char, Int] => Char
  object CharCriteria:
    val mostCommon: CharCriteria = _.maxBy(_.swap)._1
    val leastCommon: CharCriteria = _.minBy(_.swap)._1

  def solvePart1(input: List[String]): Int = {
    val length = input.headOption.fold(0)(_.length)

    def calculateRate(criteria: CharCriteria): Int =
      toInt((0 until length).map(selectChar(input, _, criteria)).mkString)

    val gamma = calculateRate(CharCriteria.mostCommon)
    val epsilon = calculateRate(CharCriteria.leastCommon)

    gamma * epsilon
  }

  def solvePart2(input: List[String]): Int = {

    def calculateRating(criteria: CharCriteria): Int = {
      @tailrec
      def loop(i: Int, input: List[String]): String = input match
        case single :: Nil => single
        case many =>
          val target = selectChar(input, i, criteria)
          loop(i + 1, many.filter(_.charAt(i) == target))

      toInt(loop(0, input))
    }

    val oxygen = calculateRating(CharCriteria.mostCommon)
    val co2 = calculateRating(CharCriteria.leastCommon)

    oxygen * co2
  }

  private def selectChar(input: List[String], atIndex: Int, criteria: CharCriteria) =
    criteria(count(input.map(_.charAt(atIndex))))

  private def toInt(binary: String) = if (binary.isEmpty) 0 else Integer.parseInt(binary, 2)

  @main def solveDay3(): Unit =
    val input = Resources.lines("day3.txt")
    println(solvePart1(input)) // 3885894
    println(solvePart2(input)) // 4375225
