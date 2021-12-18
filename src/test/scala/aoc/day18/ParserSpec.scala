package aoc.day18

import aoc.day18.SnailFishNumber
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ParserSpec extends AnyFlatSpec with Matchers {

  for {
    (input, expected) <- List(
      ("[1,2]", SnailFishNumber(1, 2)),
      ("[[1,2],3]", SnailFishNumber(SnailFishNumber(1, 2), 3)),
      ("[9,[8,7]]", SnailFishNumber(9, SnailFishNumber(8, 7))),
      ("[[1,9],[8,5]]", SnailFishNumber(SnailFishNumber(1, 9), SnailFishNumber(8, 5))),
      (
        "[[[[1,2],[3,4]],[[5,6],[7,8]]],9]",
        SnailFishNumber(
          SnailFishNumber(
            SnailFishNumber(SnailFishNumber(1, 2), SnailFishNumber(3, 4)),
            SnailFishNumber(SnailFishNumber(5, 6), SnailFishNumber(7, 8))
          ),
          9
        )),
      (
        "[[[9,[3,8]],[[0,9],6]],[[[3,7],[4,9]],3]]",
        SnailFishNumber(
          SnailFishNumber(SnailFishNumber(9, SnailFishNumber(3, 8)), SnailFishNumber(SnailFishNumber(0, 9), 6)),
          SnailFishNumber(SnailFishNumber(SnailFishNumber(3, 7), SnailFishNumber(4, 9)), 3)
        )
      ),
      (
        "[[[[1,3],[5,3]],[[1,3],[8,7]]],[[[4,9],[6,9]],[[8,2],[7,3]]]]",
        SnailFishNumber(
          SnailFishNumber(
            SnailFishNumber(SnailFishNumber(1, 3), SnailFishNumber(5, 3)),
            SnailFishNumber(SnailFishNumber(1, 3), SnailFishNumber(8, 7))
          ),
          SnailFishNumber(
            SnailFishNumber(SnailFishNumber(4, 9), SnailFishNumber(6, 9)),
            SnailFishNumber(SnailFishNumber(8, 2), SnailFishNumber(7, 3))
          )
        )
      )
    )
  } {
    it should s"parse: $input" in {
      Parser.parseSnailNumber(input) shouldBe expected
    }

    it should s"parse and format back to string: $input" in {
      Parser.parseSnailNumber(input).toString shouldBe input
    }
  }
}
