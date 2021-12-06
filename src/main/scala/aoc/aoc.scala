package aoc

def count[A](iterable: Iterable[A]): Map[A, Int] =
  iterable.groupBy(identity).view.mapValues(_.size).toMap
