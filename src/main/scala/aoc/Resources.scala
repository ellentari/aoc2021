package aoc

import scala.io.Source
import scala.util.Using

object Resources {

  def lines(path: String): List[String] =
    Using(Source.fromResource(path))(_.getLines().toList).get

  def string(path: String): String =
    lines(path).mkString("\n")

}