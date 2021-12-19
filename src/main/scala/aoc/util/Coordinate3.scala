package aoc.util

case class Coordinate3(x: Int, y: Int, z: Int):
  def +(c: Coordinate3): Coordinate3 = Coordinate3(x + c.x, y + c.y, z + c.z)
  def -(c: Coordinate3): Coordinate3 = Coordinate3(x - c.x, y - c.y, z - c.z)
  override def toString: String = s"($x,$y,$z)"
