package aoc.day18

import cats.syntax.all.*

import scala.annotation.tailrec

object Parser:

  enum Token:
    case OpenBracket
    case CloseBracket
    case Coma
    case Number(value: Int)

  enum StackToken:
    case NewPair
    case Single(value: Int)
    case PairInProgress(left: StackToken)
    case Pair(left: StackToken, right: StackToken)

  import StackToken.*
  import Token.*

  def parseSnailNumber(line: String): SnailFishNumber = {
    @tailrec
    def evaluate(tokens: List[Token], stack: List[StackToken]): Either[String, StackToken] =
      (tokens, stack) match
        case (Nil, top :: Nil) => Right(top)
        case (OpenBracket :: remaining, _) =>
          evaluate(remaining, NewPair :: stack)
        case (Coma :: remaining, top :: bottom) =>
          evaluate(remaining, PairInProgress(top) :: bottom)
        case (CloseBracket :: remaining, top :: NewPair :: bottom) =>
          evaluate(remaining, completePairs(top :: bottom))
        case (Number(value) :: remaining, NewPair :: _) =>
          evaluate(remaining, Single(value) :: stack)
        case (Number(value) :: remaining, PairInProgress(left) :: bottom) =>
          evaluate(remaining, Pair(left, Single(value)) :: bottom)
        case _ =>
          Left(s"Unexpected state. Remaining tokens: $tokens. Stack: $stack.")

    @tailrec
    def completePairs(stack: List[StackToken]): List[StackToken] = stack match
      case right :: PairInProgress(left) :: tail =>
        completePairs(Pair(left, right) :: tail)
      case _ => stack

    line.split("").toList
      .traverse(parseToken)
      .flatMap(evaluate(_, Nil))
      .flatMap(tokenToSnailNumber)
      .fold(error => throw new RuntimeException("Parsing failed: " + error), identity)
  }

  private def parseToken(token: String): Either[String, Token] =
    token match
      case "[" => Right(OpenBracket)
      case "]" => Right(CloseBracket)
      case "," => Right(Coma)
      case number => number.toIntOption.map(Number(_)).toRight("Not a valid number: " + number)

  private def tokenToSnailNumber(token: StackToken): Either[String, SnailFishNumber] = token match
    case Single(value) => Right(SnailFishNumber(value))
    case Pair(left, right) =>
      for {
        leftNum <- tokenToSnailNumber(left)
        rightNum <- tokenToSnailNumber(right)
      } yield SnailFishNumber(leftNum, rightNum)
    case _ => Left("Unexpected token encountered: " + token)
