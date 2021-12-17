package aoc.day16

case class Packet(version: Int, data: PacketData)

enum Operator:
  case Sum, Product, Min, Max, GreaterThan, LessThan, EqualTo

enum PacketData:
  case Literal(value: Long)
  case SubPackets(op: Operator, packets: List[Packet])

enum OperatorLengthType:
  case Length(value: Int)
  case NumberOfPackets(value: Int)

enum PacketType:
  case Literal
  case Op(value: Operator)