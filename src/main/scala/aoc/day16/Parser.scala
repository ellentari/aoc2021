package aoc.day16

case class Location(input: String, globalOffset: Int):
  def next(consumed: Int): Location =
    Location(input.substring(consumed), globalOffset + consumed)
  def limit(length: Int): Location =
    Location(input.substring(0, length), globalOffset)

case class ParsingResult[A](consumed: Int, result: A):
  def map[B](f: A => B): ParsingResult[B] = copy(result = f(result))
  def addConsumed(moreConsumed: Int): ParsingResult[A] = copy(consumed = consumed + moreConsumed)

trait Parser[A]:
  self =>

  def parse(input: Location): Either[String, ParsingResult[A]]

  def parse(input: String): Either[String, ParsingResult[A]] = parse(Location(input, 0))

  def map[B](f: A => B): Parser[B] = self.parse(_).map(_.map(f))

  def product[B](pb: => Parser[B]): Parser[(A, B)] =
    for {
      a <- self
      b <- pb
    } yield (a, b)

  def map2[B, C](pb: => Parser[B])(f: (A, B) => C): Parser[C] = product(pb).map(f.tupled)

  def flatMap[B](f: A => Parser[B]): Parser[B] =
    input => self.parse(input).flatMap { success =>
      f(success.result).parse(input.next(success.consumed))
        .map(_.addConsumed(success.consumed))
    }

  def or(other: => Parser[A]): Parser[A] = loc => self.parse(loc).orElse(other.parse(loc))

  def many: Parser[List[A]] =
    self.map2(self.many)(_ :: _).or(Parser.success(List.empty[A]))

  def repeat(n: Int): Parser[List[A]] =
    if (n == 0) Parser.success(List.empty[A])
    else self.map2(repeat(n - 1))(_ :: _)

  def takeUntil(condition: A => Boolean, includeTerminal: Boolean = false): Parser[List[A]] =
    for {
      a <- self
      more <-
        if (condition(a)) takeUntil(condition, includeTerminal)
        else Parser.success(List.empty[A])
    } yield if (condition(a) || includeTerminal) a :: more else more

  def limitInput(length: Int): Parser[A] =
    loc => self.parse(loc.limit(length)).map(r => r.copy(consumed = length))

object Parser:
  def success[A](a: A): Parser[A] = _ => Right(ParsingResult(0, a))
  def error[A](error: String): Parser[A] = loc => Left(s"Position: ${loc.globalOffset}. $error")
