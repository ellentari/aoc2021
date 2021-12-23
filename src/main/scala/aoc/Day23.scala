package aoc

import aoc.util.{Coordinate, Grid}

import scala.annotation.tailrec
import scala.collection.immutable.Queue
import scala.collection.mutable

object Day23:

  enum AmphipodType(val moveEnergy: Int):
    case Amber extends AmphipodType(1)
    case Bronze extends AmphipodType(10)
    case Copper extends AmphipodType(100)
    case Desert extends AmphipodType(1000)

  enum Cell:
    case Amphipod(amphipodType: AmphipodType)
    case Empty
    case Wall

  case class Move(from: (Int, Int), to: (Int, Int), cost: Int)

  case class Board(grid: Grid[Cell], roomDepth: Int, totalCost: Int = 0):
    def isEmpty(c: (Int, Int)): Boolean = grid(c) == Cell.Empty

    def hallway: Set[(Int, Int)] = Set(1, 2, 4, 6, 8, 10, 11).map((1, _))
    def roomAmber: Set[(Int, Int)] = (2 until 2 + roomDepth).map((_, 3)).toSet
    def roomBronze: Set[(Int, Int)] = (2 until 2 + roomDepth).map((_, 5)).toSet
    def roomCopper: Set[(Int, Int)] = (2 until 2 + roomDepth).map((_, 7)).toSet
    def roomDesert: Set[(Int, Int)] = (2 until 2 + roomDepth).map((_, 9)).toSet
    def targetRoom(amphipodType: AmphipodType): Set[(Int, Int)] = amphipodType match
      case AmphipodType.Amber => roomAmber
      case AmphipodType.Bronze => roomBronze
      case AmphipodType.Copper => roomCopper
      case AmphipodType.Desert => roomDesert

    def applyMove(move: Move): Board = copy(
      grid = grid.updated(move.from._1, move.from._2, Cell.Empty).updated(move.to._1, move.to._2, grid(move.from)),
      totalCost = totalCost + move.cost
    )
    def isFinished: Boolean = AmphipodType.values.forall(isFinished)
    private def isFinished(amphipodType: AmphipodType): Boolean =
      targetRoom(amphipodType).map(grid(_)).forall(_ == Cell.Amphipod(amphipodType))

    def getOutOfPlaceAmphipodPositions: Map[(Int, Int), AmphipodType] = getAmphipodPositions.filterNot(isInPlace.tupled)
    def getAmphipodPositions: Map[(Int, Int), AmphipodType] =
      grid.indices
        .map(p => p -> grid(p))
        .collect { case (p, Cell.Amphipod(amphipodType)) => p -> amphipodType }
        .toMap

    private def isInPlace(position: (Int, Int), amphipodType: AmphipodType): Boolean =
      targetRoom(amphipodType).contains(position) && below(position).forall(p => grid(p) == Cell.Amphipod(amphipodType))
    private def below(position: (Int, Int)): List[(Int, Int)] =
      (position._1 + 1 until grid.height).map(_ -> position._2).takeWhile(p => grid(p) != Cell.Wall).toList

    def tryReach(amphipodType: AmphipodType, from: (Int, Int), to: (Int, Int)): Option[Int] =
      @tailrec
      def bfs(queue: Queue[((Int, Int), Int)], seen: Set[(Int, Int)]): Option[Int] = queue.dequeueOption match
        case None => None
        case Some(((position, distance), tail)) =>
          if (position == to)
            Some(distance * amphipodType.moveEnergy)
          else
            val next = grid.adjacent(position._1, position._2)
              .filter(isEmpty)
              .filterNot(seen.contains)
            bfs(tail ++ next.map(_ -> (distance + 1)), seen ++ next)

      if (!isEmpty(to)) None
      else bfs(Queue((from, 0)), Set(from))
  end Board

  def solvePart1(initial: Board): Int =
    val priorityQueue = mutable.PriorityQueue.empty[Board](Ordering.by(-_.totalCost))
    val seen = mutable.HashMap.empty[Map[(Int, Int), AmphipodType], Int]

    priorityQueue.enqueue(initial)

    while (priorityQueue.nonEmpty)
      val board = priorityQueue.dequeue()

      if (seen.get(board.getAmphipodPositions).exists(_ < board.totalCost)) ()
      else if (board.isFinished)
        return board.totalCost
      else
        val updatedStates = possibleMoves(board)
          .map(board.applyMove)
          .filter(updState => !seen.get(updState.getAmphipodPositions).exists(_ < updState.totalCost))

        for (updState <- updatedStates)
          seen.put(updState.getAmphipodPositions, updState.totalCost)

        priorityQueue.enqueue(updatedStates*)

    -1
  end solvePart1

  private def possibleMoves(board: Board): List[Move] =
    def reachable(targetPositions: Iterable[(Int, Int)])(from: (Int, Int), amphipodType: AmphipodType) =
      targetPositions.flatMap(to => board.tryReach(amphipodType, from, to).map(cost => Move(from, to, cost)))

    def targetRoomMoves(position: (Int, Int), amphipodType: AmphipodType) =
      val targetRoom = board.targetRoom(amphipodType)
      val moveTo =
        if (targetRoom.map(board.grid(_)).forall(c => c == Cell.Empty || c == Cell.Amphipod(amphipodType)))
          targetRoom.takeWhile(c => board.grid(c) == Cell.Empty).lastOption
        else None
      reachable(moveTo)(position, amphipodType)

    def hallwayMoves = reachable(board.hallway.filter(board.isEmpty))

    val outOfPlace = board.getOutOfPlaceAmphipodPositions.toList
    val roomMoves = outOfPlace.flatMap(targetRoomMoves.tupled)

    if (roomMoves.nonEmpty) roomMoves
    else outOfPlace.filter((p, _) => !board.hallway.contains(p)).flatMap(hallwayMoves.tupled)

  private def parseInput(input: List[String], roomDepth: Int) =
    def parseValue(ch: Char) = ch match
      case 'A' => Cell.Amphipod(AmphipodType.Amber)
      case 'B' => Cell.Amphipod(AmphipodType.Bronze)
      case 'C' => Cell.Amphipod(AmphipodType.Copper)
      case 'D' => Cell.Amphipod(AmphipodType.Desert)
      case '.' => Cell.Empty
      case '#' | ' ' => Cell.Wall

    Board(Grid(input.map(_.map(parseValue).toIndexedSeq).toIndexedSeq), roomDepth)

  @main def solveDay23(): Unit =
    val sampleInput1 = parseInput(Resources.lines("day23_sample.txt"), 2)
    val sampleInput2 = parseInput(Resources.lines("day23_sample_part2.txt"), 4)
    val input1 = parseInput(Resources.lines("day23.txt"), 2)
    val input2 = parseInput(Resources.lines("day23_part2.txt"), 4)

    println(solvePart1(sampleInput1)) // 12521
    println(solvePart1(input1)) // 13558
    println(solvePart1(sampleInput2)) // 44169
    println(solvePart1(input2)) // 56982
