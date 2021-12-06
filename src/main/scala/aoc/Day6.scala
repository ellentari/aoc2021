package aoc

object Day6:

  private val CycleLength = 7
  private val InitialDelay = 2
  private val VectorSize = CycleLength + InitialDelay

  def solvePart1(input: List[Int]): Long = solve(input, iterations = 80)

  def solvePart2(input: List[Int]): Long = solve(input, iterations = 256)

  private def solve(input: List[Int], iterations: Int): Long =
    val initialCount = count(input)
    (0 until iterations)
      .foldLeft((0 until VectorSize).map(initialCount.get(_).fold(0L)(_.toLong))) { (timers, _) =>
        val newbornCount = timers.head
        val shiftedTimers = timers.tail

        shiftedTimers.updated(CycleLength - 1, shiftedTimers(CycleLength - 1) + newbornCount) :+ newbornCount
      }
      .sum

  @main def solveDay6(): Unit =
    val input = Resources.string("day6.txt").split(",").map(_.toInt).toList
    println(solvePart1(input)) // 360610
    println(solvePart2(input)) // 1631629590423
