package aoc

import scala.math.Ordering.Implicits.*

object Day5:

  final case class Coordinate(x: Int, y: Int)
  object Coordinate:
    implicit val orderingCoordinate: Ordering[Coordinate] = Ordering.by(l => (l.x, l.y))

  final case class Line private(left: Coordinate, right: Coordinate):
    def maxX: Int = right.x
    def maxY: Int = right.y max left.y

    def isHorizontal: Boolean = left.y == right.y
    def isVertical: Boolean = left.x == right.x

    def coordinates: Iterable[Coordinate] =
      if (isHorizontal || isVertical)
        ys.flatMap(y => xs.map(Coordinate(_, y)))
      else
        xs.zip(ys).map(Coordinate.apply)

    private def xs: Range = left.x to right.x
    private def ys: Range =
      val delta = if (left.y < right.y) 1 else -1
      left.y to right.y by delta

  object Line:
    def from(c1: Coordinate, c2: Coordinate): Line = Line(c1 min c2, c1 max c2)

  def solvePart1(input: List[Line]): Int = solve(input.filter(l => l.isHorizontal || l.isVertical))

  def solvePart2(input: List[Line]): Int = solve(input)

  private def solve(lines: List[Line]): Int =
    lines
      .flatMap(_.coordinates)
      .groupMapReduce(identity)(_ => 1)(_ + _)
      .count(_._2 >= 2)

  private def parseLine(s: String): Line = s match
    case s"$x1,$y1 -> $x2,$y2" =>
      val c1 = Coordinate(x1.toInt, y1.toInt)
      val c2 = Coordinate(x2.toInt, y2.toInt)
      Line.from(c1, c2)

  @main def solveDay5(): Unit =
    val input = Resources.lines("day5.txt").map(parseLine)
    println(solvePart1(input)) // 5373
    println(solvePart2(input)) // 21514
