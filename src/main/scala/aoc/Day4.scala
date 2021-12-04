package aoc

import scala.annotation.tailrec

object Day4:

  opaque type Number = Int

  case class Cell(number: Number, marked: Boolean = false)

  case class BingoBoard (cells: IndexedSeq[IndexedSeq[Cell]]):
    def isBingo: Boolean = cells.exists(_.forall(_.marked)) || cells.transpose.exists(_.forall(_.marked))

    def unmarkedNumbersSum: Number = cells.flatten.filterNot(_.marked).map(_.number).sum

    def tryMark(number: Number): Option[BingoBoard] =
      cells.zipWithIndex
        .flatMap((row, i) => row.zipWithIndex.map((cell, j) => (cell, i, j)))
        .find(_._1.number == number)
        .map((cell, i, j) => copy(cells = cells.updated(i, cells(i).updated(j, cell.copy(marked = true)))))

  case class BingoGame(terminationCondition: BingoGame.State => Option[BingoBoard]):
    def play(input: BingoGame.Input): BingoGame.Score =
      @tailrec def loop(state: BingoGame.State): Option[BingoGame.Score] =
        nextState(state) match
          case None => None
          case Some((lastNumber, nextState)) =>
            terminationCondition(nextState) match
              case Some(board) => Some(BingoGame.calculateScore(board, lastNumber))
              case None => loop(nextState)

      loop(BingoGame.State(input.numbers, input.boards)).getOrElse(BingoGame.Score.Zero)

    private def nextState(state: BingoGame.State) = state.numbers match
      case Nil => None
      case number :: remainingNumbers =>
        val (remainingPlayingBoards, newFinishedBoards) = state.playingBoards
          .partitionMap { board =>
            board.tryMark(number) match {
              case Some(updatedBoard) if updatedBoard.isBingo => Right(updatedBoard)
              case Some(updatedBoard) => Left(updatedBoard)
              case None => Left(board)
            }
          }

        Some(number -> BingoGame.State(remainingNumbers, remainingPlayingBoards, state.finishedBoards ++ newFinishedBoards))

  object BingoGame:
    opaque type Score = Int
    object Score:
      val Zero: Score = 0

    def calculateScore(board: BingoBoard, lastNumber: Number): Score = board.unmarkedNumbersSum * lastNumber

    case class Input(numbers: List[Number], boards: List[BingoBoard])

    case class State(numbers: List[Number], playingBoards: List[BingoBoard], finishedBoards: List[BingoBoard] = Nil):
      def firstWinner: Option[BingoBoard] = finishedBoards.headOption
      def lastWinner: Option[BingoBoard] = Option.when(playingBoards.isEmpty)(finishedBoards.lastOption).flatten

  def solvePart1: BingoGame.Input => BingoGame.Score = BingoGame(_.firstWinner).play

  def solvePart2: BingoGame.Input => BingoGame.Score = BingoGame(_.lastWinner).play

  private def parseInput(input: String): BingoGame.Input =
    def parseInputNumbers(raw: String) = raw.split(",").map(_.toInt).toList
    def parseBingoBoard(raw: String): BingoBoard =
      BingoBoard(raw
        .split("\n")
        .map(_.trim.split("\\s+").map(_.toInt).map(Cell(_)).toIndexedSeq)
        .toIndexedSeq)

    val parts = input.split("\n\n")
    val numbers = parseInputNumbers(parts.head)
    val boards = parts.tail.map(parseBingoBoard).toList

    BingoGame.Input(numbers, boards)

  @main def solveDay4(): Unit =
    val input = parseInput(Resources.string("day4.txt"))
    println(solvePart1(input)) // 44088
    println(solvePart2(input)) // 23670
