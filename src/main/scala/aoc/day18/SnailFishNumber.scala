package aoc.day18

sealed trait SnailFishNumber:
  def +(other: SnailFishNumber): SnailFishNumber = SnailFishNumberAlgebra.add(this, other)
  def magnitude: Int = SnailFishNumberAlgebra.magnitude(this)

object SnailFishNumber:
  
  case class Regular(value: Int) extends SnailFishNumber :
    override def toString: String = value.toString
  case class Pair(left: SnailFishNumber, right: SnailFishNumber) extends SnailFishNumber :
    override def toString: String = "[" + left.toString + "," + right.toString + "]"

  def apply(value: Int): SnailFishNumber = Regular(value)
  def apply(left: Int, right: Int): SnailFishNumber = Pair(SnailFishNumber(left), SnailFishNumber(right))
  def apply(left: Int, right: SnailFishNumber): SnailFishNumber = Pair(SnailFishNumber(left), right)
  def apply(left: SnailFishNumber, right: Int): SnailFishNumber = Pair(left, SnailFishNumber(right))
  def apply(left: SnailFishNumber, right: SnailFishNumber): SnailFishNumber = Pair(left, right)
  
end SnailFishNumber
