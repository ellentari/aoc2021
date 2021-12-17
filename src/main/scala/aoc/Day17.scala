package aoc

import aoc.util.Coordinate

import scala.annotation.tailrec

object Day17:

  case class Area(bottomLeft: Coordinate, topRight: Coordinate):
    def contains(c: Coordinate): Boolean =
      bottomLeft.x <= c.x && c.x <= topRight.x && bottomLeft.y <= c.y && c.y <= topRight.y

  case class Velocity(x: Int, y: Int)

  case class LaunchResult(initialVelocity: Velocity, maxY: Int)

  def solvePart1(target: Area): Int = findInitialVelocities(target).map(_.maxY).maxOption.getOrElse(0)

  def solvePart2(target: Area): Int = findInitialVelocities(target).size

  private def findInitialVelocities(target: Area): List[LaunchResult] = {
    @tailrec
    def move(probe: Coordinate, velocity: Velocity, maxY: Int): (Boolean, Int) = {
      val newProbe = Coordinate(x = probe.x + velocity.x, y = probe.y + velocity.y)
      val newVelocity = Velocity(
        x = if (velocity.x > 0) velocity.x - 1 else if (velocity.x < 0) velocity.x + 1 else 0,
        y = velocity.y - 1
      )

      if (target.contains(newProbe)) (true, maxY)
      else if (newProbe.y < target.bottomLeft.y || target.topRight.x < newProbe.x) (false, maxY)
      else move(newProbe, newVelocity, maxY max newProbe.y)
    }

    (for {
      xVelocity <- 0 to target.topRight.x
      yVelocity <- target.bottomLeft.y to -target.bottomLeft.y
      velocity = Velocity(xVelocity, yVelocity)
      (passesTarget, maxY) = move(Coordinate(0, 0), velocity, 0) if passesTarget
    } yield LaunchResult(velocity, maxY)).toList
  }

  private def parseTargetArea(s: String): Area = s match
    case s"target area: x=$x1..$x2, y=$y1..$y2" =>
      Area(Coordinate(x1.toInt, y1.toInt), Coordinate(x2.toInt, y2.toInt))

  @main def solveDay17(): Unit =
    val sampleInput = parseTargetArea("target area: x=20..30, y=-10..-5")
    val input = parseTargetArea("target area: x=211..232, y=-124..-69")

    println(solvePart1(sampleInput)) // 45
    println(solvePart1(input)) // 7626
    println(solvePart2(sampleInput)) // 112
    println(solvePart2(input)) // 2032
