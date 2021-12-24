package aoc

object Day24:

  case class Program(divZ: IndexedSeq[Int], addX: IndexedSeq[Int], addY: IndexedSeq[Int]):
    def blocksCount: Int = List(divZ, addX, addY).map(_.size).min

    /*
      Simplified code for:
        inp w
        mul x 0
        add x z
        mod x 26
        div z divZ[block]
        add x addX[block]
        eql x w
        eql x 0
        mul y 0
        add y 25
        mul y x
        add y 1
        mul z y
        mul y 0
        add y w
        add y addY[block]
        mul y x
        add z y
    */
    def executeBlock(input: Int, initialZ: Long, blockNumber: Int): Long =
      val x = initialZ % 26 + addX(blockNumber)
      val z = initialZ / divZ(blockNumber)

      if (x == input) z
      else z * 26 + input + addY(blockNumber)

  def solvePart1: Program => Option[Long] = findValidInput(inputRange = 9 to 1 by -1)

  def solvePart2: Program => Option[Long] = findValidInput(inputRange = 1 to 9)

  private def findValidInput(inputRange: Range)(program: Program): Option[Long] =
    val seen = scala.collection.mutable.HashSet.empty[(Long, Int)]

    def loop(inputZ: Long, block: Int, totalInput: Long): Option[Long] =
      if (block == program.blocksCount)
        Option.when(inputZ == 0)(totalInput)
      else if (!seen.contains((inputZ, block)))
          seen.add((inputZ, block))
          inputRange.view
            .flatMap { input =>
              val outputZ = program.executeBlock(input, inputZ, block)
              loop(outputZ, block + 1, totalInput * 10 + input)
            }
            .headOption
      else None

    loop(inputZ = 0L, block = 0, totalInput = 0L)

  private def parseProgram(input: List[String]) =
    val inputs = input
      .grouped(18)
      .toIndexedSeq.transpose
      .filter(_.distinct.size > 1)
      .map(_.map(_.split(" ")(2).toInt).toIndexedSeq)
    Program(inputs(0), inputs(1), inputs(2))

  @main def solveDay24(): Unit =
    val program = parseProgram(Resources.lines("day24.txt"))

    println(solvePart1(program)) // 99911993949684
    println(solvePart2(program)) // 62911941716111
