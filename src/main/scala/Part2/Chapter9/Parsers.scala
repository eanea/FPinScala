package Part2.Chapter9

import Part2.Chapter8.{Gen, Prop, SGen}

import scala.util.matching.Regex

trait Parsers { self =>
  type Parser[+A] = Location => Result[A]
  sealed trait Result[+A] {
    def mapError(f: ParseError => ParseError): Result[A] = this match {
      case Failure(e) => Failure(f(e))
      case _          => this
    }
  }
  case class Success[+A](get: A, charsConsumed: Int) extends Result[A]
  case class Failure(get: ParseError)                extends Result[Nothing]

  def run[A](p: Parser[A])(input: String): Either[ParseError, A]
  def char(c: Char): Parser[Char] =
    string(c.toString) map (_.charAt(0))
  def string(s: String): Parser[String] = { (loc: Location) =>
    if (loc.input.startsWith(s, loc.offset)) Success(s, s.length)
    else Failure(ParseError(List(loc -> s"expected: $s")))

  }
  def or[A](p1: Parser[A], p2: => Parser[A]): Parser[A]

  /**
    * EXERCISE 9.4
    */
  def listOfN[A](n: Int, p: Parser[A]): Parser[List[A]] =
    if (n <= 0) succeed(List.empty[A])
    else
      map2(p, listOfN(n - 1, p))(_ :: _)

  def listOfNFill[A](n: Int, p: Parser[A]): Parser[List[A]] = p.map(a => List.fill(n)(a))

  /**
    * EXERCISE 9.3
    */
  def many[A](p: Parser[A]): Parser[List[A]] = map2(p, many(p))(_ :: _) or succeed(List.empty[A])

  /**
    * EXERCISE 9.8
    */
  def map[A, B](p: Parser[A])(f: A => B): Parser[B] = p.flatMap(a => succeed(f(a)))

  def succeed[A](a: A): Parser[A] = s => Success(a, 0)

  /**
    * EXERCISE 9.13
    */
  def slice[A](p: Parser[A]): Parser[String] = { (loc: Location) =>
    p(loc) match {
      case f @ Failure(get) => f
      case Success(get, charsConsumed) =>
        Success(loc.input.substring(loc.offset, loc.offset + charsConsumed), charsConsumed)
    }
  }

  def many1[A](p: Parser[A]): Parser[List[A]] = map2(p, many(p))(_ :: _)

  /**
    * EXERCISE 9.7
    */
  def product[A, B](p: Parser[A], p2: => Parser[B]): Parser[(A, B)] =
    for {
      a <- p
      b <- p2
    } yield (a, b)
  def map2ViaFlatMap[A, B, C](p: Parser[A], p2: => Parser[B])(f: (A, B) => C): Parser[C] =
    for {
      a <- p
      b <- p2
    } yield f(a, b)

  def skipL[B](p: Parser[Any], p2: => Parser[B]): Parser[B] =
    map2(slice(p), p2)((_, b) => b)

  def skipR[A](p: Parser[A], p2: => Parser[Any]): Parser[A] =
    map2(p, slice(p2))((a, b) => a)

  def surround[A](start: Parser[Any], stop: Parser[Any])(p: => Parser[A]) =
    start *> p <* stop

  def sep[A](p: Parser[A], p2: Parser[Any]): Parser[List[A]] =
    sep1(p, p2) or succeed(List())

  def sep1[A](p: Parser[A], p2: Parser[Any]): Parser[List[A]] =
    map2(p, many(p2 *> p))(_ :: _)

  /**
    * EXERCISE 9.1
    */
  def map2[A, B, C](p: Parser[A], p2: => Parser[B])(f: (A, B) => C): Parser[C] = map(product(p, p2))(t => f(t._1, t._2))

  def flatMap[A, B](p: Parser[A])(f: A => Parser[B]): Parser[B]

  def attempt[A](p: Parser[A]): Parser[A]

  def whitespace: Parser[String] = "\\s*".r

  def token[A](p: Parser[A]): Parser[A] =
    attempt(p) <* whitespace

  implicit def asString(s: String): Parser[String]
  implicit def operators[A](p: Parser[A]): ParserOps[A]                                    = ParserOps[A](p)
  implicit def asStringParser[A](a: A)(implicit f: A => Parser[String]): ParserOps[String] = ParserOps(f(a))
  implicit def regex(r: Regex): Parser[String] = { (loc: Location) =>
    r.findPrefixOf(loc.input.substring(loc.offset)) match {
      case Some(value) => Success(value, value.length)
      case None        => Failure(ParseError(List(loc -> s"$r doesn't match")))
    }
  }

  case class ParserOps[A](p: Parser[A]) {
    def |[B >: A](p2: Parser[B]): Parser[B]       = self.or(p, p2)
    def or[B >: A](p2: => Parser[B]): Parser[B]   = self.or(p, p2)
    def many: Parser[List[A]]                     = self.many(p)
    def map[B](f: A => B): Parser[B]              = self.map(p)(f)
    def succeed(a: A): Parser[A]                  = self.succeed(a)
    def slice: Parser[String]                     = self.slice(p)
    def many1: Parser[List[A]]                    = self.many1(p)
    def product[B](p2: Parser[B]): Parser[(A, B)] = self.product(p, p2)
    def **[B](p2: Parser[B]): Parser[(A, B)]      = self.product(p, p2)
    def flatMap[B](f: A => Parser[B]): Parser[B]  = self.flatMap(p)(f)
    def *>[B](p2: => Parser[B])                   = self.skipL(p, p2)
    def <*(p2: => Parser[Any])                    = self.skipR(p, p2)
    def token                                     = self.token(p)
    def sep(separator: Parser[Any])               = self.sep(p, separator)
    def sep1(separator: Parser[Any])              = self.sep1(p, separator)

    def label(msg: String): Parser[A] = self.label(msg)(p)
    def scope(msg: String): Parser[A] = self.scope(msg)(p)
    def as[B](b: B): Parser[B]        = self.map(self.slice(p))(_ => b)
  }

  def label[A](msg: String)(p: Parser[A]): Parser[A] =
    loc => p(loc).mapError(_.label(msg))

  def scope[A](msg: String)(p: Parser[A]): Parser[A] =
    loc => p(loc).mapError(_.push(loc, msg))

  case class Location(input: String, offset: Int = 0) {
    lazy val line = input.slice(0, offset + 1).count(_ == '\n') + 1
    lazy val col = input.slice(0, offset + 1).lastIndexOf('\n') match {
      case -1        => offset + 1
      case lineStart => offset - lineStart
    }

    def toError(msg: String): ParseError =
      ParseError(List((this, msg)))
  }

  def errorLocation(e: ParseError): Location
  def errorMessage(e: ParseError): String

  case class ParseError(stack: List[(Location, String)]) {
    def push(loc: Location, msg: String): ParseError =
      copy(stack = (loc, msg) :: stack)

    def label[A](s: String): ParseError =
      ParseError(latestLoc.map((_, s)).toList)

    def latestLoc: Option[Location] =
      latest map (_._1)

    def latest: Option[(Location, String)] =
      stack.lastOption
  }

  object Laws {
    def equal[A](p1: Parser[A], p2: Parser[A])(in: Gen[String]): Prop =
      Prop.forAll(in)(s => run(p1)(s) == run(p2)(s))
    def mapLaw[A](p: Parser[A])(in: Gen[String]): Prop =
      equal(p, p.map(a => a))(in)
    def succeedLaw[A](p: Parser[A], a: A)(in: Gen[String]): Prop = equal(p, p.succeed(a))(in)

    /**
      * EXERCISE 9.2
      * Associative
      * a ** (b ** c) == (a ** b) ** c
      */
    def labelLaw[A](p: Parser[A], inputs: SGen[String]): Prop =
      Prop.forAll(inputs ** Gen.string) {
        case (input, msg) =>
          run(label(msg)(p))(input) match {
            case Left(e) => errorMessage(e) == msg
            case _       => true
          }
      }
  }
}
