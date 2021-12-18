package aoc.day18

import aoc.day18.SnailFishNumber.*

import scala.annotation.tailrec

object SnailFishNumberAlgebra:

  enum ExplosionResult:
    case Unchanged
    case Exploded(updatedNumber: SnailFishNumber, left: Int, right: Int)
    case LeftMissing(updatedNumber: SnailFishNumber, left: Int)
    case RightMissing(updatedNumber: SnailFishNumber, right: Int)
    case Updated(updatedNumber: SnailFishNumber)

  import ExplosionResult.*

  def magnitude(number: SnailFishNumber): Int = number match
    case Regular(value) => value
    case Pair(left, right) => 3 * magnitude(left) + 2 * magnitude(right)

  def add(n1: SnailFishNumber, n2: SnailFishNumber): SnailFishNumber = reduce(Pair(n1, n2))

  def reduce(number: SnailFishNumber): SnailFishNumber =
    @tailrec
    def loop(number: SnailFishNumber): SnailFishNumber =
      explode(number).orElse(split(number)) match
        case None => number
        case Some(updated) => loop(updated)

    loop(number)

  def split(number: SnailFishNumber): Option[SnailFishNumber] =
    number match
      case Regular(value) if value >= 10 =>
        Some(SnailFishNumber(value / 2, (value.toDouble / 2).ceil.toInt))
      case Regular(_) => None
      case Pair(left, right) =>
        split(left) match
          case Some(updLeft) => Some(Pair(updLeft, right))
          case None => split(right) match
            case Some(updRight) => Some(Pair(left, updRight))
            case None => None

  def explode(num: SnailFishNumber): Option[SnailFishNumber] =
    explodeRec(num, 0) match
      case Unchanged => None
      case Updated(value) => Some(value)
      case LeftMissing(value, _) => Some(value)
      case RightMissing(value, _) => Some(value)
      case Exploded(_, _, _) => throw new IllegalStateException("Should not be the case!")

  private def explodeRec(number: SnailFishNumber, nesting: Int): ExplosionResult =
    number match
      case Regular(_) => Unchanged
      case Pair(Regular(left), Regular(right)) if nesting == 4 =>
        Exploded(Regular(0), left, right)
      case Pair(left, right) =>
        explodeRec(left, nesting + 1) match
          case Exploded(updatedLeft, addLeft, addRight) =>
            val updatedRight = addToLeftmostSingle(right, addRight)
            LeftMissing(Pair(updatedLeft, updatedRight), addLeft)
          case RightMissing(updatedLeft, addRight) =>
            val updatedRight = addToLeftmostSingle(right, addRight)
            Updated(Pair(updatedLeft, updatedRight))
          case Updated(updatedLeft) =>
            Updated(Pair(updatedLeft, right))
          case LeftMissing(updatedLeft, addLeft) =>
            LeftMissing(Pair(updatedLeft, right), addLeft)
          case Unchanged =>
            explodeRec(right, nesting + 1) match
              case Exploded(updatedRight, addLeft, addRight) =>
                val updatedLeft = addToRightmostSingle(left, addLeft)
                RightMissing(Pair(updatedLeft, updatedRight), addRight)
              case LeftMissing(updatedRight, addLeft) =>
                val updatedLeft = addToRightmostSingle(left, addLeft)
                Updated(Pair(updatedLeft, updatedRight))
              case RightMissing(updatedRight, addRight) =>
                RightMissing(Pair(left, updatedRight), addRight)
              case Updated(updatedRight) =>
                Updated(Pair(left, updatedRight))
              case Unchanged => Unchanged

  private def addToLeftmostSingle(number: SnailFishNumber, amount: Int): SnailFishNumber =
    number match
      case Regular(value) => Regular(value + amount)
      case s@Pair(x, _) => s.copy(left = addToLeftmostSingle(x, amount))

  private def addToRightmostSingle(number: SnailFishNumber, amount: Int): SnailFishNumber =
    number match
      case Regular(value) => Regular(value + amount)
      case s@Pair(_, y) => s.copy(right = addToRightmostSingle(y, amount))
