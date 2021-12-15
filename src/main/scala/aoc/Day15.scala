package aoc

import scala.collection.mutable
import aoc.util.Grid

object Day15:

  def solvePart1(input: Grid[Int]): Int = lowestTotalRisk(input)

  def solvePart2(input: Grid[Int]): Int = lowestTotalRisk(expand(input))

  private def lowestTotalRisk(riskLevels: Grid[Int]): Int =
    val totalRisk = Array.fill(riskLevels.height, riskLevels.width)(Int.MaxValue)
    totalRisk(0)(0) = 0

    val done = Array.ofDim[Boolean](riskLevels.height, riskLevels.width)
    val pq = mutable.PriorityQueue((0, 0, 0))(Ordering.by(-_._3))

    while (pq.nonEmpty) {
      val (i, j, _) = pq.dequeue()

      if (!done(i)(j)) {
        val currentRisk = totalRisk(i)(j)
        val risksToUpdate = riskLevels.adjacent(i, j)
          .filterNot((i1, j1) => done(i1)(j1))
          .map((i1, j1) => (i1, j1, currentRisk + riskLevels(i1)(j1)))
          .filter((i1, j1, c) => c < totalRisk(i1)(j1))

        for ((i1, j1, c) <- risksToUpdate)
          totalRisk(i1)(j1) = c

        pq.enqueue(risksToUpdate *)
        done(i)(j) = true
      }
    }

    totalRisk.last.last
  end lowestTotalRisk

  private def expand(grid: Grid[Int]) =
    val height = grid.height
    val width = grid.width

    Grid((0 until height * 5)
      .map(i => (0 until width * 5)
        .map(j => {
          val value = grid(i % height)(j % width) + (i / height + j / width)

          if (value % 9 == 0) 9 else value % 9
        })))

  private def parseGrid(input: List[String]): Grid[Int] = Grid(input.map(_.map(_ - '0')).toIndexedSeq)

  @main def solveDay15(): Unit =
    val sampleInput = parseGrid(Resources.lines("day15_sample.txt"))
    val input = parseGrid(Resources.lines("day15.txt"))

    println(solvePart1(sampleInput)) // 40
    println(solvePart1(input)) // 562
    println(solvePart2(sampleInput)) // 315
    println(solvePart2(input)) // 2874
