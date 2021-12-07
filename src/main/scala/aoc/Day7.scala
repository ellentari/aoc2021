package aoc

object Day7:

  def solvePart1: List[Int] => Int = solve(medianOption.andThen(_.toList), absDiff)
  def solvePart2: List[Int] => Int = solve(meanOption.andThen(_.toList.flatMap(m => List(m.floor, m.ceil).map(_.toInt))),
    (a, b) => triangularNum(absDiff(a, b)))

  private def solve(alignTo: List[Int] => List[Int], alignmentCost: (Int, Int) => Int)(positions: List[Int]): Int =
    def totalAlignmentCost(alignTo: Int): Int = positions.map(alignmentCost(_, alignTo)).sum

    alignTo(positions).map(totalAlignmentCost).minOption.getOrElse(0)

  private def absDiff(from: Int, to: Int): Int = (from - to).abs
  private def triangularNum(n: Int): Int = n * (n + 1) / 2

  @main def solveDay7(): Unit =
    val sampleInput = "16,1,2,0,4,2,7,1,2,14".split(",").map(_.toInt).toList
    val input = Resources.string("day7.txt").split(",").map(_.toInt).toList

    println(solvePart1(sampleInput)) // 37
    println(solvePart1(input)) // 343605
    println(solvePart2(sampleInput)) // 168
    println(solvePart2(input)) // 96744904
