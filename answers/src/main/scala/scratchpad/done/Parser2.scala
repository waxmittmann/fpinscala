//class Parser[S]
//class ParseError

package scratchpad.parser2

import fpinscala.testing.{Prop, Gen}

import scala.util.matching.Regex

/*
    Laws = p(x) | p(y)

 */

object Main {
  def main(args: Array[String]) = {
    println("Hi")
  }
}

trait Parsers[ParseError, Parser[+_]] { self =>
  implicit def operators[A](p: Parser[A]) = ParserOps[A](p)
  implicit def asStringParser[A](a: A)(implicit f: A => Parser[String]): ParserOps[String] = ParserOps(f(a))

  def char(c: Char): Parser[Char] = {
    string(c.toString).map(_.charAt(0))
  }

  def succeed[A](a: A): Parser[A] = {
    string("") map (_ => a)
  }


  def string(s: String): Parser[String] = ???

  def run[A](p: Parser[A])(input: String): Either[ParseError, A] = ???
  def or[A](s1: Parser[A], s2: => Parser[A]): Parser[A] = ???

  def listOfN[A](n: Int, p: Parser[A]): Parser[List[A]] = {
    if (n <= 0)
      succeed(List())
    else
      map2(p, listOfN(n-1, p))(_ :: _)
  }

  def many[A](p: Parser[A]): Parser[List[A]] = {
    map2(p, many(p))((a: A, la: List[A]) => a :: la) | succeed(List())
  }

  def manyAChars(): Parser[Int] = char('a').many().slice().map(_.length)

  def manyNr[A](p: Parser[A]) = map(many(p))(_.length)

  def slice[A](p: Parser[A]): Parser[String] = ???

//  def product[A,B](p: Parser[A], p2: => Parser[B]): Parser[(A,B)] = ???

  def many1[A](p: Parser[A]): Parser[List[A]] = map2(p, p.many())(_ :: _)

  def map2B[A, B, C](p1: Parser[A], p2: => Parser[B])(f: (A, B) => C): Parser[C] = map(product(p1, p2))(f.tupled)

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

//    p.flatMap((r1: A) => {
//      p2.flatMap((r2: B) => {
//        succeed((r1, r2))
//      })
//    })
  }

  def regex(regex: Regex): Parser[List[String]] = ???

  def flatMap[A, B](p: Parser[A])(f: A => Parser[B]): Parser[B] = ???

//  val p: Parser[Int]
//  flatMap(p)((a: Int) => {
//    succeed(a) ** listOfN(a, p)
//  })

  def map[A,B](p: Parser[A])(f: A => B): Parser[B] = {
    p.flatMap(a => succeed(f(a)))
  }

  def parseBasedOnFirst[A](p: Parser[A]): Parser[List[A]] = {
    val rootParser: Parser[List[String]] = regex("([0-9]*)(.*)".r)
    flatMap(rootParser)((strings: List[String]) => {
      val times = Integer.valueOf(strings(0))
      listOfN(times, p)
    })
//    rootParser.flatMap((strings: List[String]) => {
//
//    })
  }

  object Laws {
    def equal[A](p1: Parser[A], p2: Parser[A])(in: Gen[String]): Prop =
      Prop.forAll(in)(s =>
        run(p1)(s) == run(p2)(s))

    def mapLaw[A](p: Parser[A])(in: Gen[String]): Prop =
      equal(p, p.map((a: A) => a))(in)

    def unitLaw[A](p: Parser[A])(in: Gen[String]): Prop =
      Prop.forAll(in)(s =>
        run(succeed(s))(s) == Right(s))

//    def associativeLaw[A](pa: Parser[A], pb: Parser[A], pc: Parser[A])(fa: X => A, fb: X => B, fc: X => C)(in: Gen[X]): Prop = {
    def associativeLaw[A](pa: Parser[A], pb: Parser[A], pc: Parser[A])(in: Gen[String]): Prop = {
      Prop.forAll(in)(s => {
        val p1: Parser[(A, A, A)] = ((pa ** pb) ** pc).map((r: ((A, A), A)) => (r._1._1, r._1._2, r._2))
        val p2: Parser[(A, A, A)] = (pa ** (pb ** pc)).map((r: (A, (A, A))) => (r._1, r._2._1, r._2._2))
        run(p1)(s) == run(p2)(s)
      })
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
}

