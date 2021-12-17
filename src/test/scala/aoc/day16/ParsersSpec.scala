package aoc.day16

import aoc.day16.Day16.parsePacket
import aoc.day16.Operator.{LessThan, Max}
import aoc.day16.PacketData.{Literal, SubPackets}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ParsersSpec extends AnyFlatSpec with Matchers {

  behavior of "Parsers.packet"
  private val packetParser = Parsers.packet

  it should "parse literal" in {
    packetParser.parse("110100101111111000101000") shouldBe Right(ParsingResult(21, Packet(6, Literal(2021))))
  }

  it should "parse operator with length type ID = 0" in {
    packetParser.parse("00111000000000000110111101000101001010010001001000000000") shouldBe Right(
      ParsingResult(49, Packet(1, SubPackets(LessThan, List(Packet(6, Literal(10)), Packet(2, Literal(20)))))))
  }

  it should "parse operator with length type ID = 1" in {
    packetParser.parse("11101110000000001101010000001100100000100011000001100000") shouldBe Right(
      ParsingResult(51, Packet(7, SubPackets(Max, List(Packet(2, Literal(1)), Packet(4, Literal(2)), Packet(1, Literal(3)))))))
  }

}
