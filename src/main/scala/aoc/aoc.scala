package aoc

def count[A](iterable: Iterable[A]): Map[A, Int] =
  iterable.groupBy(identity).view.mapValues(_.size).toMap

def countLong[A](iterable: Iterable[A]): Map[A, Long] =
  iterable.groupBy(identity).view.mapValues(_.size.toLong).toMap

def medianOption(input: Iterable[Int]): Option[Int] =
  if (input.isEmpty) None
  else
    val sortedInput = input.toIndexedSeq.sorted
    if (sortedInput.size % 2 != 0) Some(sortedInput(sortedInput.length / 2))
    else
      val (lower, higher) = sortedInput.splitAt(sortedInput.length / 2)
      meanOption(List(lower.last, higher.head)).map(_.round.toInt)

def meanOption(input: Iterable[Int]): Option[Double] = Option.when(input.nonEmpty)(input.sum.toDouble / input.size)
  
