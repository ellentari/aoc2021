package aoc

object Day1:

  def solvePart1(input: List[Int]): Int =
    input
      .sliding(2)
      .count {
        case a :: b :: Nil if a < b => true
        case _ => false
      }

  def solvePart2(input: List[Int]): Int = solvePart1(input.sliding(3).map(_.sum).toList)

  @main def solveDay1(): Unit =
    val input = Resources.lines("day1.txt").map(_.toInt)
    println(solvePart1(input)) // 1715
    println(solvePart2(input)) // 1739
