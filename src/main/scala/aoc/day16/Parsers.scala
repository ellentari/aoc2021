package aoc.day16

import aoc.day16.Operator.*
import aoc.day16.PacketData.*

object Parsers:

  def string(length: Int, description: String = "string"): Parser[String] =
    loc =>
      if (loc.input.length < length)
        Left(s"Position ${loc.globalOffset}. Expected $description to be more than $length characters long.")
      else Right(ParsingResult(length, loc.input.substring(0, length)))

  def bitStringInt(length: Int, name: String): Parser[Int] =
    string(length, name).map(Integer.parseInt(_, 2))

  def packet: Parser[Packet] =
    for {
      version <- packetVersion
      pType <- packetType
      data <- pType match
        case PacketType.Literal => literal
        case PacketType.Op(op) => operator(op)
    } yield Packet(version, data)

  def packetVersion: Parser[Int] = bitStringInt(3, "version")

  def packetType: Parser[PacketType] =
    for {
      id <- bitStringInt(3, "packet type ID")
      result <- id match
        case 0 => Parser.success(PacketType.Op(Sum))
        case 4 => Parser.success(PacketType.Literal)
        case 1 => Parser.success(PacketType.Op(Product))
        case 2 => Parser.success(PacketType.Op(Min))
        case 3 => Parser.success(PacketType.Op(Max))
        case 5 => Parser.success(PacketType.Op(GreaterThan))
        case 6 => Parser.success(PacketType.Op(LessThan))
        case 7 => Parser.success(PacketType.Op(EqualTo))
        case _ => Parser.error("Unexpected packet type ID encountered: " + id)
    } yield result

  def literal: Parser[PacketData.Literal] =
    string(5).takeUntil(_.startsWith("1"), includeTerminal = true)
      .map(_.map(_.tail).mkString(""))
      .map(java.lang.Long.parseLong(_, 2))
      .map(PacketData.Literal.apply)

  def operator(op: Operator): Parser[PacketData.SubPackets] =
    for {
      lengthType <- operatorLengthType
      subPackets <- lengthType match
        case OperatorLengthType.Length(length) => packet.many.limitInput(length)
        case OperatorLengthType.NumberOfPackets(n) => packet.repeat(n)
    } yield SubPackets(op, subPackets)

  def operatorLengthType: Parser[OperatorLengthType] =
    for {
      id <- bitStringInt(1, "length type ID")
      result <- id match
        case 0 => bitStringInt(15, "length").map(OperatorLengthType.Length(_))
        case 1 => bitStringInt(11, "number of packets").map(OperatorLengthType.NumberOfPackets(_))
        case _ => Parser.error("Unexpected length type ID encountered: " + id)
    } yield result
