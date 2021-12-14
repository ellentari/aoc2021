package aoc

import cats.syntax.all.*

object Day14:

  type Element = Char

  case class Input(polymerTemplate: String, insertionRules: Map[Pair, Element])

  case class Pair(first: Element, second: Element):
    def insert(element: Element): List[Pair] = List(Pair(first, element), Pair(element, second))

  case class Polymer(pairCount: Map[Pair, Long], elementCount: Map[Element, Long]):
    def mostCommonElementCount: Long = elementCount.values.maxOption.getOrElse(0L)
    def leastCommonElementCount: Long = elementCount.values.minOption.getOrElse(0L)

    def expand(insertionRules: Map[Pair, Element]): Polymer =
      val pairsToReplace = (pairCount.keySet & insertionRules.keySet).toList
      val addedPairs = pairsToReplace
        .foldMap(pair => pair.insert(insertionRules(pair)).map(_ -> pairCount(pair)).toMap)
      val addedElements = pairsToReplace.foldMap(pair => Map(insertionRules(pair) -> pairCount(pair)))

      Polymer((pairCount -- pairsToReplace) |+| addedPairs, elementCount |+| addedElements)
    end expand

  object Polymer:
    def fromString(s: String): Polymer =
      val pairs = s.sliding(2).toList.map(p => Pair(p(0), p(1)))
      Polymer(countLong(pairs), countLong(s))

  def solvePart1: Input => Long = solve(steps = 10)

  def solvePart2: Input => Long = solve(steps = 40)

  private def solve(steps: Int)(input: Input): Long =
    val initialPolymer = Polymer.fromString(input.polymerTemplate)
    val finalPolymer = (0 until steps).foldLeft(initialPolymer)((p, _) => p.expand(input.insertionRules))

    finalPolymer.mostCommonElementCount - finalPolymer.leastCommonElementCount

  private def parseInput(input: List[String]): Input =
    val polymer = input.head
    val rules = input.drop(2)
      .collect {
        case s"$pair -> $element" if pair.length == 2 && element.length == 1 =>
          Pair(pair(0), pair(1)) -> element(0)
      }
      .toMap

    Input(polymer, rules)

  @main def solveDay14(): Unit =
    val sampleInput = parseInput(Resources.lines("day14_sample.txt"))
    val input = parseInput(Resources.lines("day14.txt"))

    println(solvePart1(sampleInput)) // 1588
    assert(solvePart1(sampleInput) == 1588L)
    println(solvePart1(input)) // 3831
    assert(solvePart1(input) == 3831L)
    println(solvePart2(sampleInput)) // 2188189693529
    assert(solvePart2(sampleInput) == 2188189693529L) // 2188189693529
    println(solvePart2(input)) // 5725739914282
    assert(solvePart2(input) == 5725739914282L)
