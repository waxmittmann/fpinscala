//class Parser[S]
//class ParseError

package scratchpad.parser4

/*
import fpinscala.testing.{SGen, Prop, Gen}
import scratchpad.parser4.Main.{Location, Parser, ParseError}

import scala.util.matching.Regex

object Main {
  def main(args: Array[String]) = {
    println("Hi")
  }

  type Parser[+A] = String => Result

}

case class Location(input: String, offset: Int = 0) {
  lazy val line = input.slice(0,offset+1).count(_ == '\n') + 1
  lazy val col = input.slice(0,offset+1).lastIndexOf('\n') match {
    case -1 => offset + 1
    case lineStart => offset - lineStart
  }
  def toError(msg: String): ParseError = ParseError(List((this, msg)))
}

//type Parser[+A] = Location => Result[A]
case class ParseError(stack: List[(Location,String)]) {
  def push(loc: Location, msg: String): ParseError =
    copy(stack = (loc,msg) :: stack)

  def label[A](s: String): ParseError =
    ParseError(latestLoc.map(_, s)).toList

  def latestLoc: Option[Location] =
    latest map(_._1)

  def latest: Option[(Location, String)] =
    stack.lastOption
}

trait Result[+A] {
  def mapError(f: ParseError => ParseError): Result[A] = this match {
    case Failure(e) => Failure(f(e))
    case _ => this
  }
}

case class Success[+A](get: A, charsConsumed: Int) extends Result[A]
case class Failure(get: ParseError) extends Result[Nothing]

trait Parsers[Parser[+_]] { self =>
//  type Parser = Location => Result

  implicit def operators[A](p: Parser[A]) = ParserOps[A](p)
  implicit def asStringParser[A](a: A)(implicit f: A => Parser[String]): ParserOps[String] = ParserOps(f(a))

//  def char(c: Char): Parser[Char] = {
//    string(c.toString).map(_.charAt(0))
//  }

  // Primitives
  def string[A](s: String): Parser[String]

  def run[A](p: Parser[A])(input: String): Result

  def or[A](s1: Parser[A], s2: => Parser[A]): Parser[A]

  def regex(regex: Regex): Parser[String]

  def flatMap[A, B](p: Parser[A])(f: A => Parser[B]): Parser[B]

  def slice[A](p: Parser[A]): Parser[String]

  //Compounds
  def succeed[A](a: A): Parser[A] = {
    string("").map[A]((_: String) => a)
  }

  def listOfN[A](n: Int, p: Parser[A]): Parser[List[A]] = {
    if (n <= 0)
      succeed(List())
    else
      map2(p, listOfN(n-1, p))(_ :: _)
  }

  def many[A](p: Parser[A]): Parser[List[A]] = {
    map2(p, many(p))((a: A, la: List[A]) => a :: la) | succeed(List())
  }

  def many1[A](p: Parser[A]): Parser[List[A]] = map2(p, p.many())(_ :: _)

  def countMany[A](p: Parser[A]) = map(many(p))(_.length)

  def map2[A, B, C](p1: Parser[A], p2: => Parser[B])(f: (A, B) => C): Parser[C] = {
    for {
      r1 <- p1
      r2 <- p2
    } yield f(r1, r2)
  }

  def product[A,B](p: Parser[A], p2: => Parser[B]): Parser[(A,B)] = {
    for {
      r1 <- p
      r2 <- p2
    } yield (r1, r2)
  }

  def map[A,B](p: Parser[A])(f: A => B): Parser[B] = {
    p.flatMap(a => succeed(f(a)))
  }

  //Delay commit to parse
  def attempt[A](p: Parser[A]): Parser[A]

  object Laws {
    def equal[A](p1: Parser[A], p2: Parser[A])(in: Gen[String]): Prop =
      Prop.forAll(in)(s =>
        run(p1)(s) == run(p2)(s))

    def mapLaw[A](p: Parser[A])(in: Gen[String]): Prop =
      equal(p, p.map((a: A) => a))(in)

    def unitLaw[A](p: Parser[A])(in: Gen[String]): Prop =
      Prop.forAll(in)(s =>
        run(succeed(s))(s) == Right(s))

    def associativeLaw[A](pa: Parser[A], pb: Parser[A], pc: Parser[A])(in: Gen[String]): Prop = {
      Prop.forAll(in)(s => {
        val p1: Parser[(A, A, A)] = ((pa ** pb) ** pc).map((r: ((A, A), A)) => (r._1._1, r._1._2, r._2))
        val p2: Parser[(A, A, A)] = (pa ** (pb ** pc)).map((r: (A, (A, A))) => (r._1, r._2._1, r._2._2))
        run(p1)(s) == run(p2)(s)
      })
    }

    def labelLaw[A](p: Parser[A], inputs: SGen[String]): Prop = Prop.forAll(inputs ** Gen.string) { case (input, msg) =>
      run(label(msg)(p))(input) match {
        case Left(e) => errorMessage(e) == msg case _ => true
      }
    }
  }

  case class ParserOps[A](p: Parser[A]) {
    def |[B >: A](p2: Parser[B]): Parser[B] = self.or(p, p2)

    def or[B >: A](p2: => Parser[B]): Parser[B] = self.or(p, p2)

    def times[B >: A](n: Int): Parser[List[A]] = self.listOfN(n, p)

    def **[B](pB: Parser[B]): Parser[(A, B)] = self.product(p, pB)

    def map[B](f: A => B): Parser[B] = self.map(p)(f)

    def slice(): Parser[String] = self.slice(p)

    def many(): Parser[List[A]] = self.many(p)

    def flatMap[B](f: A => Parser[B]): Parser[B] = self.flatMap(p)(f)
  }

  def label[A](msg: String)(p: Parser[A]): Parser[A]
  def scope[A](msg: String)(p: Parser[A]): Parser[A]
  def errorLocation(e: ParseError): Location
  def errorMessage(e: ParseError): String
}


//class MyParser[+A] {
//
//}


//type Parser[+A]

object MyParsers extends Parsers[Parser] { // implementations of primitives go here

  // Primitives
  override def string[A](s: String): Parser[String] = {
    (input: String) =>
      if (input.startsWith(s)) {
        Right(s)
      } else {
        Left(Location(input).toError("Expected: " + s))
      }
  }

  override def flatMap[A, B](p: Parser[A])(f: (A) => Parser[B]): Parser[B] = ???

  override def or[A](s1: Parser[A], s2: => Parser[A]): Parser[A] = ???

  override def errorMessage(e: ParseError): String = ???

  //override def scope[A](msg: String)(p: Parser[A]): Parser[A] = ???

  override def run[A](p: Parser[A])(input: String): Result = ???

  override def errorLocation(e: ParseError): Location = ???

  override def regex(regex: Regex): Parser[String] = {
    val r: Parser[String] = (input: String) =>
      input match {
        case regex() => Failure(Location(input).toError("Expected: " + regex.regex))
        case default => Success(input, input.length)
      }
    r
  }

  def scope[A](msg: String)(p: Parser[A]): Parser[A] = {
//    s => {
//      val a: Result = p(s)
//    }

    s => p(s).mapError(_.push(s.loc, msg))
  }

  override def slice[A](p: Parser[A]): Parser[String] = ???

  override def label[A](msg: String)(p: Parser[A]): Parser[A] =
    s => p(s).mapError(_.label(msg))

  //Delay commit to parse
  override def attempt[A](p: Parser[A]): Parser[A] = ???

}
*/