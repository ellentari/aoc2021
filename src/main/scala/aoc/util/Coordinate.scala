package aoc.util

case class Coordinate(x: Int, y: Int)

object Coordinate:
  implicit val orderingCoordinate: Ordering[Coordinate] = Ordering.by(l => (l.x, l.y))
