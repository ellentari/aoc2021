package aoc

import aoc.util.Coordinate3

import scala.annotation.tailrec

object Day22:

  case class Cuboid(start: Coordinate3, end: Coordinate3, removed: List[Cuboid] = Nil):
    def volume: Long =
      (end.x - start.x + 1).toLong * (end.y - start.y + 1) * (end.z - start.z + 1) -
        removed.map(_.volume).sum

    def intersect(other: Cuboid): Option[Cuboid] =
      def intersect(s1: Int, e1: Int, s2: Int, e2: Int) =
        Option.unless(s2 > e1 || s1 > e2) {
          val sorted = IndexedSeq(s1, e1, s2, e2).sorted
          (sorted(1), sorted(2))
        }
      for {
        (x1, x2) <- intersect(start.x, end.x, other.start.x, other.end.x)
        (y1, y2) <- intersect(start.y, end.y, other.start.y, other.end.y)
        (z1, z2) <- intersect(start.z, end.z, other.start.z, other.end.z)
      } yield Cuboid(Coordinate3(x1, y1, z1), Coordinate3(x2, y2, z2))

    def remove(other: Cuboid): Cuboid =
      intersect(other) match
        case None => this
        case Some(intersection) =>
          copy(removed = intersection :: removed.map(_.remove(intersection)))
  end Cuboid

  def solvePart1(input: List[(Cuboid, Boolean)]): Long =
    def withinRange(cub: Cuboid): Boolean =
      List(cub.start.x, cub.end.x, cub.start.y, cub.end.y, cub.start.z, cub.end.z).forall(_.abs <= 50)
    solvePart2(input.filter((region, _) => withinRange(region)))

  def solvePart2(input: List[(Cuboid, Boolean)]): Long =
    @tailrec
    def loop(remaining: List[(Cuboid, Boolean)], on: List[Cuboid]): List[Cuboid] =
      remaining match
        case Nil => on
        case (region, isOn) :: tail =>
          loop(tail, on.map(_.remove(region)) ++ Option.when(isOn)(region))

    loop(input, Nil).map(_.volume).sum

  private def parseCuboid(s: String) = s match
    case s"$instruction x=$x1..$x2,y=$y1..$y2,z=$z1..$z2" =>
      val on = instruction == "on"
      Cuboid(Coordinate3(x1.toInt, y1.toInt, z1.toInt), Coordinate3(x2.toInt, y2.toInt, z2.toInt)) -> on

  @main def solveDay22(): Unit =
    val sampleInput1 = Resources.lines("day22_sample_1.txt").map(parseCuboid)
    val sampleInput2 = Resources.lines("day22_sample_2.txt").map(parseCuboid)
    val sampleInput3 = Resources.lines("day22_sample_3.txt").map(parseCuboid)
    val input = Resources.lines("day22.txt").map(parseCuboid)

    println(solvePart1(sampleInput1)) // 39
    println(solvePart1(sampleInput2)) // 590784
    println(solvePart1(input)) // 607657
    println(solvePart2(sampleInput3)) // 2758514936282235
    println(solvePart2(input)) // 1187742789778677
