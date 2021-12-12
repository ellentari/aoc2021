package aoc

object Day12:

  type Graph = Map[Cave, List[Cave]]

  case class Cave(name: String):
    def isSmall: Boolean = name.toLowerCase == name
    def isBig: Boolean = !isSmall

  object Cave:
    val Start: Cave = Cave("start")
    val End: Cave = Cave("end")

  case class State(visited: Set[Cave] = Set.empty, revisited: Option[Cave] = None):
    def addVisited(cave: Cave): State = copy(
      visited = visited + cave,
      revisited =
        if (!notVisited(cave) && canRevisit(cave)) Some(cave)
        else revisited
    )

    def notVisited(cave: Cave): Boolean = !visited.contains(cave)
    def canRevisit(cave: Cave): Boolean = cave match
      case Cave.Start | Cave.End => false
      case _ => revisited.isEmpty
  end State

  def solvePart1: Graph => Int = countPaths(_.notVisited(_))

  def solvePart2: Graph => Int = countPaths((s, c) => s.notVisited(c) || s.canRevisit(c))

  private def countPaths(shouldVisit: (State, Cave) => Boolean)(graph: Graph): Int =
    def dfs(cave: Cave, state: State): Int =
      if (cave == Cave.End) 1
      else
        graph.getOrElse(cave, Nil)
          .filter(shouldVisit(state, _))
          .map(c => dfs(c, if (c.isSmall) state.addVisited(c) else state))
          .sum

    dfs(Cave.Start, State().addVisited(Cave.Start))
  end countPaths

  private def parseGraph(input: List[String]): Graph =
    val edgeList = input.map { case s"$a-$b" => (Cave(a), Cave(b)) }
    edgeList.flatMap(edge => List(edge, edge.swap)).groupMap(_._1)(_._2)

  @main def solveDay12(): Unit =
    val sampleInput1 = Resources.lines("day12_sample_1.txt")
    val sampleInput2 = Resources.lines("day12_sample_2.txt")
    val sampleInput3 = Resources.lines("day12_sample_3.txt")
    val input = Resources.lines("day12.txt")

    println(solvePart1(parseGraph(sampleInput1))) // 10
    println(solvePart1(parseGraph(sampleInput2))) // 19
    println(solvePart1(parseGraph(sampleInput3))) // 226
    println(solvePart1(parseGraph(input))) // 4573
    println(solvePart2(parseGraph(sampleInput1))) // 36
    println(solvePart2(parseGraph(sampleInput2))) // 103
    println(solvePart2(parseGraph(sampleInput3))) // 3509
    println(solvePart2(parseGraph(input))) // 117509
