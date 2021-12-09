package aoc

import scala.annotation.tailrec
import scala.collection.immutable.Queue
import scala.util.Try

object Day9 {

  final case class HeatMap(value: IndexedSeq[IndexedSeq[Int]]):
    def indices: List[(Int, Int)] = value.indices.flatMap(i => value(i).indices.map(i -> _)).toList
    def adjacentIndices(i: Int, j: Int): List[(Int, Int)] =
      List((i, j + 1), (i + 1, j), (i, j - 1), (i - 1, j))
        .filter((i1, j1) => i1 >= 0 && i1 < value.length && j1 >= 0 && j1 < value(i1).length)

    def riskLevel(i: Int, j: Int): Int = value(i)(j) + 1
    def isLowPoint(i: Int, j: Int): Boolean = value(i)(j) < adjacentIndices(i, j).map((i, j) => value(i)(j)).min

    def lowPoints: List[(Int, Int)] = indices.filter(isLowPoint.tupled)
  end HeatMap

  def solvePart1(heatMap: HeatMap): Int = heatMap.lowPoints.map(heatMap.riskLevel.tupled).sum

  def solvePart2(heatMap: HeatMap): Int =
    heatMap.lowPoints.map(discoverBasin(heatMap).tupled).sortBy(-_).take(3).product

  private def discoverBasin(heatMap: HeatMap)(i: Int, j: Int): Int =
    @tailrec
    def bfs(queue: Queue[(Int, Int)], seen: Set[(Int, Int)]): Int =
      queue.dequeueOption match
        case None => seen.size
        case Some(((i, j), tail)) =>
          val toVisit = heatMap.adjacentIndices(i, j)
            .filterNot(seen.contains)
            .filter((i1, j1) => heatMap.value(i1)(j1) < 9)
          bfs(tail ++ toVisit, seen ++ toVisit)

    bfs(Queue(i -> j), Set(i -> j))
  end discoverBasin

  private def parseHeatMap(raw: List[String]): HeatMap =
    HeatMap(raw.toIndexedSeq.map(_.toIndexedSeq.map(_ - '0')))

  @main def solveDay9(): Unit =
    val input = parseHeatMap(Resources.lines("day9.txt"))
    println(solvePart1(input)) // 600
    println(solvePart2(input)) // 987840

}
