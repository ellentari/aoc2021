package aoc.day16

import aoc.Resources
import aoc.day16.Day16
import aoc.day16.Day16.*
import aoc.day16.Operator.*
import aoc.day16.PacketData.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class Day16Spec extends AnyFlatSpec with Matchers {

  behavior of "solvePart1"

  for {
    (hex, expected) <- List(
      "8A004A801A8002F478" -> 16,
      "620080001611562C8802118E34" -> 12,
      "C0015000016115A2E0802F182340" -> 23,
      "A0016C880162017C3686B18A3D4780" -> 31
    )
  } it should s"add up version numbers: $hex" in {
    solvePart1(hex) shouldBe expected
  }

  it should "solve real input" in {
    val input = Resources.string("day16.txt")
    solvePart1(input) shouldBe 953
  }

  behavior of "solvePart2"

  for {
    (hex, operator, expected) <- List(
      ("C200B40A82", "sum", 3),
      ("04005AC33890", "product", 54),
      ("880086C3E88112", "minimum", 7),
      ("CE00C43D881120", "maximum", 9),
      ("D8005AC2A8F0", "less than", 1),
      ("F600BC2D8F", "not greater than", 0),
      ("9C005AC2F8F0", "not equal to", 0),
      ("9C0141080250320F1802104A08", "equal to", 1)
    )
  } it should s"evaluate [$operator] expression" in {
    solvePart2(hex) shouldBe expected
  }

  it should "solve real input" in {
    val input = Resources.string("day16.txt")
    solvePart2(input) shouldBe 246225449979L
  }

}
