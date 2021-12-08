package aoc

import scala.annotation.tailrec

object Day8:

  case class Input(signalPatterns: List[Segments], output: List[Segments])

  case class Segments(value: Set[Char]):
    def size: Int = value.size
    def contains(other: Segments): Boolean = (other.value -- value).isEmpty
    override def toString: String = value.mkString

  def solvePart1(input: List[Input]): Int =
    input.flatMap(_.output).count(pat => Set(2, 4, 3, 7).contains(pat.size))

  def solvePart2(inputs: List[Input]): Int =
    def calculateOutput(input: Input): Int =
      val decodedDigits = decode(input.signalPatterns.toSet)
      input.output.map(decodedDigits).mkString.toInt

    inputs.map(calculateOutput).sum

  private def decode(signalPatterns: Set[Segments]): Map[Segments, Int] =
    def findUnique(patterns: Set[Segments], predicate: Segments => Boolean) =
      val filtered = patterns.filter(predicate)
      Option.when(filtered.size == 1)(filtered.head).get

    // 1, 4, 7, 8 have unique segment length
    val one = findUnique(signalPatterns, _.size == 2)
    val four = findUnique(signalPatterns, _.size == 4)
    val seven = findUnique(signalPatterns, _.size == 3)
    val eight = findUnique(signalPatterns, _.size == 7)

    // 0, 6, 9 have segment length = 6
    val zeroSixNine = signalPatterns.filter(_.size == 6)
    // 9 contains 4
    val nine = findUnique(zeroSixNine, _.contains(four))
    // 0 contains 1
    val zero = findUnique(zeroSixNine - nine, _.contains(one))
    // remaining is 6
    val six = findUnique(zeroSixNine - nine - zero, _ => true)

    // 2, 3, 5 have segment length = 5
    val twoThreeFive = signalPatterns.filter(_.size == 5)
    // 3 contains 1
    val three = findUnique(twoThreeFive, _.contains(one))
    // 5 is included in nine
    val five = findUnique(twoThreeFive - three, nine.contains)
    // remaining is 2
    val two = findUnique(twoThreeFive - three - five, _ => true)

    Map(zero -> 0, one -> 1, two -> 2, three -> 3, four -> 4, five -> 5, six -> 6, seven -> 7, eight -> 8, nine -> 9)
  end decode

  private def parseInput(s: String): Input =
    def parsePatterns(raw: String) = raw.split("\\s+").toList.map(s => Segments(s.toSet))

    s match
      case s"$firstPart | $secondPart" =>
        Input(parsePatterns(firstPart), parsePatterns(secondPart))

  @main def solveDay8(): Unit =
    val sampleInput = Resources.lines("day8_sample.txt").map(parseInput)
    val input = Resources.lines("day8.txt").map(parseInput)

    println(solvePart1(sampleInput)) // 26
    println(solvePart1(input)) // 255
    println(solvePart2(
      List(parseInput("acedgfb cdfbe gcdfa fbcad dab cefabd cdfgeb eafb cagedb ab | cdfeb fcadb cdfeb cdbaf")))) // 5353
    println(solvePart2(sampleInput)) // 61229
    println(solvePart2(input)) // 982158
