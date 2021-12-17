package aoc.day16

import aoc.Resources

import java.lang.RuntimeException

object Day16:

  import Operator.*
  import PacketData.*

  def solvePart1(hex: String): Int =
    def versionSum(packet: Packet): Int = packet.data match
      case Literal(_) => packet.version
      case SubPackets(_, subPackets) => packet.version + subPackets.map(versionSum).sum

    versionSum(parsePacket(hexToBinary(hex)))

  def solvePart2(hex: String): Long =
    def evaluate(packet: Packet): Long = packet.data match
      case Literal(value) => value
      case SubPackets(Sum, sub) => sub.map(evaluate).sum
      case SubPackets(Product, sub) => sub.map(evaluate).product
      case SubPackets(Min, sub) => sub.map(evaluate).min
      case SubPackets(Max, sub) => sub.map(evaluate).max
      case SubPackets(GreaterThan, List(first, second)) => if (evaluate(first) > evaluate(second)) 1L else 0L
      case SubPackets(LessThan, List(first, second)) => if (evaluate(first) < evaluate(second)) 1L else 0L
      case SubPackets(EqualTo, List(first, second)) => if (evaluate(first) == evaluate(second)) 1L else 0L
      case _ => throw new RuntimeException(s"Failed to evaluate packet: $packet")

    evaluate(parsePacket(hexToBinary(hex)))

  private def parsePacket(binary: String): Packet =
    Parsers.packet.parse(binary).fold(error => throw new RuntimeException(error), _.result)

  private def hexToBinary(hex: String): String =
    hex.map {
      case '0' => "0000"
      case '1' => "0001"
      case '2' => "0010"
      case '3' => "0011"
      case '4' => "0100"
      case '5' => "0101"
      case '6' => "0110"
      case '7' => "0111"
      case '8' => "1000"
      case '9' => "1001"
      case 'A' => "1010"
      case 'B' => "1011"
      case 'C' => "1100"
      case 'D' => "1101"
      case 'E' => "1110"
      case 'F' => "1111"
    }.mkString

  @main def solveDay16(): Unit =
    val input = Resources.string("day16.txt")

    println(solvePart1(input)) // 953
    println(solvePart2(input)) // 246225449979
