package aoc

import scala.annotation.tailrec
import scala.collection.mutable

object Day21:

  private val TrackLength = 10

  private val MaxScorePt1 = 1000
  private val MaxScorePt2 = 21

  private val DiceSidesPt1 = 100
  private val DiceSidesPt2 = 3

  case class PlayersState(player1: PlayerState, player2: PlayerState)

  case class PlayerState(position: Int, score: Int = 0):
    def next(rolled: Int): PlayerState =
      val newPosition = (position + rolled) % TrackLength
      PlayerState(newPosition, score + newPosition + 1)

  case class Counter(player1Wins: Long, player2Wins: Long):
    def +(other: Counter): Counter = Counter(player1Wins + other.player1Wins, player2Wins + other.player2Wins)
    def swap: Counter = Counter(player2Wins, player1Wins)
    def maxWins: Long = player1Wins max player2Wins

  def solvePart1(initial: PlayersState): Int =
    @tailrec
    def loop(state: PlayersState, dieRolls: Int, player1Turn: Boolean): Int =
      if (state.player1.score >= MaxScorePt1) state.player2.score * dieRolls
      else if (state.player2.score >= MaxScorePt1) state.player1.score * dieRolls
      else
        val rolled = (0 until 3).map(_ + dieRolls).map(d => (d % DiceSidesPt1) + 1).sum
        val nextState =
          if (player1Turn) state.copy(player1 = state.player1.next(rolled))
          else state.copy(player2 = state.player2.next(rolled))

        loop(nextState, dieRolls + 3, !player1Turn)

    loop(initial, dieRolls = 0, player1Turn = true)

  def solvePart2(initial: PlayersState): Long =
    val memo = mutable.HashMap.empty[PlayersState, Counter]

    def loop(state: PlayersState): Counter =
      if (state.player1.score >= MaxScorePt2) Counter(1L, 0L)
      else if (state.player2.score >= MaxScorePt2) Counter(0L, 1L)
      else if (memo.contains(state)) memo(state)
      else
        val dieRolls = 1 to DiceSidesPt2

        val result = (for {
          d1 <- dieRolls
          d2 <- dieRolls
          d3 <- dieRolls
          rolled = d1 + d2 + d3
        } yield loop(PlayersState(state.player2, state.player1.next(rolled))))
          .reduce(_ + _)
          .swap

        memo.update(state, result)
        result
    end loop

    loop(initial).maxWins
  end solvePart2

  private def parseInput(input: List[String]) =
    def parsePosition(s: String) = s match
      case s"Player $p starting position: $pos" => pos.toInt

    val p1Position = parsePosition(input.head) - 1
    val p2Position = parsePosition(input.drop(1).head) - 1

    PlayersState(PlayerState(p1Position), PlayerState(p2Position))

  @main def solveDay21(): Unit =
    val sampleInput = parseInput(Resources.lines("day21_sample.txt"))
    val input = parseInput(Resources.lines("day21.txt"))

    println(solvePart1(sampleInput)) // 739785
    println(solvePart1(input)) // 671580
    println(solvePart2(sampleInput)) // 444356092776315
    println(solvePart2(input)) // 912857726749764
