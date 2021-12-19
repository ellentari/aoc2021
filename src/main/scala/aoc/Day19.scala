package aoc

import aoc.util.Coordinate3

import scala.annotation.tailrec
import scala.collection.immutable.Queue

object Day19:

  case class ScannerData(coordinates: Set[Coordinate3]):
    def map(f: Coordinate3 => Coordinate3): ScannerData = ScannerData(coordinates.map(f))
    def size: Int = coordinates.size
    def intersect(other: ScannerData): ScannerData = ScannerData(coordinates.intersect(other.coordinates))
    def addAll(other: List[ScannerData]): ScannerData = ScannerData(coordinates ++ other.flatMap(_.coordinates))

  case class Overlap(scannerLocation: Coordinate3, scannerData: ScannerData)

  def solvePart1And2(input: List[ScannerData]): (Int, Int) =
    def manhattanDistance(c1: Coordinate3, c2: Coordinate3) =
      (c1.x - c2.x).abs + (c1.y - c2.y).abs + (c1.z - c2.z).abs

    val (fullMap, scannerLocations) = solve(input)

    val p1 = fullMap.size
    val p2 = scannerLocations.combinations(2)
      .map { case List(d1, d2) => manhattanDistance(d1, d2) }
      .max

    (p1, p2)

  private def solve(input: List[ScannerData]) =
    @tailrec
    def loop(
      queue: Queue[ScannerData],
      unknown: Set[ScannerData],
      scannerLocations: List[Coordinate3],
      fullMap: ScannerData): (ScannerData, List[Coordinate3]) =
      queue.dequeueOption match
        case None => (fullMap, scannerLocations)
        case Some((scanner, tail)) =>
          val overlapped = unknown.view
            .flatMap(s => rotations(s).view
              .flatMap(overlap(scanner, _))
              .headOption
              .map(s -> _))
            .toList

          if (overlapped.nonEmpty)
            println(s"Found: ${overlapped.size}. Remaining: ${unknown.size - overlapped.size}")

          loop(
            tail ++ overlapped.map(_._2.scannerData),
            unknown -- overlapped.map(_._1),
            scannerLocations ++ overlapped.map(_._2.scannerLocation),
            fullMap.addAll(overlapped.map(_._2.scannerData)))

    loop(Queue(input.head), input.tail.toSet, List(Coordinate3(0, 0, 0)), input.head)

  private def overlap(scanner1: ScannerData, scanner2: ScannerData): Option[Overlap] =
    val possibleDistances = for {
      // should be enough to check these
      c1 <- scanner1.coordinates.take(scanner1.coordinates.size - 11)
      c2 <- scanner2.coordinates.take(scanner1.coordinates.size - 11)
    } yield c1 - c2

    possibleDistances.view
      .map(distance => (distance, scanner2.map(_ + distance)))
      .collectFirst {
        case (dist, translatedScanner2) if scanner1.intersect(translatedScanner2).size >= 12 =>
          Overlap(dist, translatedScanner2)
      }

  private def rotations(scanner: ScannerData): List[ScannerData] =
    def roll(c: Coordinate3) = Coordinate3(c.x, c.z, -c.y)
    def turn(c: Coordinate3) = Coordinate3(-c.y, c.x, c.z)

    def applyTransformations(transformations: List[Coordinate3 => Coordinate3])(scanner: ScannerData) =
      val (acc, last) = transformations.foldLeft((List.empty[ScannerData], scanner)) { case ((acc, current), transform) =>
        val next = current.map(transform)
        (next :: acc, next)
      }
      (acc.reverse, last)

    val sequence = List.fill(3)(List(roll, turn, turn, turn)).flatten
    val resetSequence = List(roll, turn, roll)

    val (batch1, batch1Last) = applyTransformations(sequence)(scanner)
    val (_, reset) = applyTransformations(resetSequence)(batch1Last)
    val (batch2, _) = applyTransformations(sequence)(reset)

    batch1 ++ batch2

  private def parseInput(input: String) =
    def parseCoordinate(s: String) = s match
      case s"$x,$y,$z" => Coordinate3(x.toInt, y.toInt, z.toInt)

    input.split("\n\n")
      .map(_.split("\n").tail.map(parseCoordinate).toSet)
      .map(ScannerData.apply)
      .toList

  @main def solveDay19(): Unit =
    val sampleInput = parseInput(Resources.string("day19_sample.txt"))
    val input = parseInput(Resources.string("day19.txt"))

    println(solvePart1And2(sampleInput)) // (79, 3621)
    println(solvePart1And2(input)) // (376, 10772)
