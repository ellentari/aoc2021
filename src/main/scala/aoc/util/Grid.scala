package aoc.util

case class Grid[A](value: IndexedSeq[IndexedSeq[A]]):
  def height: Int = value.length
  def width: Int = value.headOption.fold(0)(_.length)
  
  def apply(i: Int)(j: Int): A = value(i)(j)
  def apply(c: (Int, Int)): A = value(c._1)(c._2)
  def updated(i: Int, j: Int, a: A): Grid[A] = Grid(value.updated(i, value(i).updated(j, a)))

  def adjacent(i: Int, j: Int): List[(Int, Int)] =
    List((i, j + 1), (i + 1, j), (i, j - 1), (i - 1, j))
      .filter((i1, j1) => i1 >= 0 && i1 < value.length && j1 >= 0 && j1 < value(i1).length)
    
  def indices: List[(Int, Int)] = value.indices.flatMap(i => value(i).indices.map(i -> _)).toList

