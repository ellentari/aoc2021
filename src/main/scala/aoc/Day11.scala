package aoc

import scala.annotation.tailrec
import scala.collection.immutable.Queue

object Day11:

  case class SimulationResult(steps: Int, flashCount: Int)

  type EnergyLevel = Int
  object EnergyLevel:
    val Flashed: EnergyLevel = 0
    val FlashThreshold: EnergyLevel = 9

  case class Grid(value: Map[(Int, Int), EnergyLevel]):
    def adjacent(i: Int, j: Int): List[(Int, Int)] =
      for {
        i1 <- (-1 to 1).map(i + _).toList
        j1 <- (-1 to 1).map(j + _) if (i1, j1) != (i, j) && value.contains((i1, j1))
      } yield (i1, j1)

    def incrementEnergyLevels: Grid = Grid(value.view.mapValues(_ + 1).toMap)
    def incrementEnergyLevels(coordinates: Iterable[(Int, Int)]): Grid =
      Grid(coordinates.foldLeft(value)((g, c) => g.updatedWith(c)(v => Some(v.getOrElse(0) + 1))))

    def updated(i: Int, j: Int, level: EnergyLevel): Grid = Grid(value.updated((i, j), level))

    def toFlash: Set[(Int, Int)] = value.filter(_._2 > EnergyLevel.FlashThreshold).keySet
    def shouldFlash(i: Int, j: Int): Boolean = value.get((i, j)).exists(_ > EnergyLevel.FlashThreshold)
    def allFlashed: Boolean = value.values.forall(_ == EnergyLevel.Flashed)
  end Grid

  def solvePart1(input: Grid, steps: Int): Int = runSimulation(input)((step, _) => step >= steps).flashCount

  def solvePart2(input: Grid): Int = runSimulation(input)((_, grid) => grid.allFlashed).steps

  private def runSimulation(initial: Grid)(terminate: (Int, Grid) => Boolean): SimulationResult =
    @tailrec
    def loop(step: Int, grid: Grid, totalFlashCount: Int): SimulationResult =
      if (terminate(step, grid))
        SimulationResult(step, totalFlashCount)
      else
        val (updatedGrid, flashCount) = flash(grid.incrementEnergyLevels)
        loop(step + 1, updatedGrid, totalFlashCount + flashCount)

    loop(0, initial, 0)
  end runSimulation

  private def flash(grid: Grid): (Grid, Int) =
    @tailrec
    def loop(grid: Grid, toFlash: Queue[(Int, Int)], seenToFlash: Set[(Int, Int)]): (Grid, Int) =
      toFlash.dequeueOption match
        case None => (grid, seenToFlash.size)
        case Some(((i, j), tail)) =>
          val toUpdate = grid.adjacent(i, j).filterNot(seenToFlash.contains)
          val updatedGrid = grid.incrementEnergyLevels(toUpdate).updated(i, j, EnergyLevel.Flashed)
          val moreToFlash = toUpdate.filter(updatedGrid.shouldFlash)

          loop(updatedGrid, tail ++ moreToFlash, seenToFlash ++ moreToFlash)

    val initialToFlash = grid.toFlash
    loop(grid, Queue.from(initialToFlash), initialToFlash)
  end flash

  private def parseGrid(input: List[String]): Grid =
    Grid(input
      .map(_.map(_ - '0').toList)
      .zipWithIndex.flatMap((row, i) => row.zipWithIndex.map((v, j) => (i, j) -> v))
      .toMap)

  @main def solveDay11(): Unit =
    val sampleInput = parseGrid(Resources.lines("day11_sample.txt"))
    val input = parseGrid(Resources.lines("day11.txt"))

    println(solvePart1(sampleInput, steps = 100)) // 1656
    println(solvePart1(input, steps = 100)) // 1679
    println(solvePart2(sampleInput)) // 195
    println(solvePart2(input)) // 519
