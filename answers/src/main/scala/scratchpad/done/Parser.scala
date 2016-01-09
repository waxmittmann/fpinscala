//class Parser[S]
//class ParseError

package scratchpad.parsers

import fpinscala.testing.{Prop, Gen}

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
  def or[A](s1: Parser[A], s2: Parser[A]): Parser[A] = ???
  def listOfN[A](n: Int, p: Parser[A]): Parser[List[A]] = ???

  def map[A, B](pa: Parser[A])(f: A => B): Parser[B] = ???
//  def many[A](p: Parser[A]): Parser[List[A]] = {
//    map()
//  }

  def many[A](p: Parser[A]): Parser[List[A]] = ???

  def manyAChars(): Parser[Int] = {
//    map(many(char('a')))(_.length)
    char('a').many().map(_.length)
  }

  def manyAChars2(): Parser[Int] = {
    char('a').many().slice().map(_.length)
  }

  def manyNr[A](p: Parser[A]) = {
    map(many(p))(_.length)
  }

  def slice[A](p: Parser[A]): Parser[String] = ???

  def product[A,B](p: Parser[A], p2: Parser[B]): Parser[(A,B)] = ???

  def map2[A, B, C](p1: Parser[A], p2: Parser[B])(f: (A, B) => C): Parser[C] = {
    map(product(p1, p2))(f.tupled)
  }

  def many1[A](p: Parser[A]): Parser[List[A]] = {
//    map(product(p, p.many()))((r: (A, List[A])) => r._1 :: r._2)
    map2(p, p.many())(_ :: _)
    map2(p, p.many())((a: A, la: List[A]) => a :: la)
  }

  //  def manyA

//  def many[A](p: Parser[List[A]]): Parser[Int] = {
//
//  }

//  def zeroMore[A](p: Parser[A]): Parser[Int] = ???
//  def oneMore[A](p: Parser[A]): Either[ParseError, Parser[Int]] = ???
//  def zeroMoreOneMore[A](p1: Parser[A], p2: Parser[A]): Either[ParseError, Parser[Int, Int]] = ???

  object Laws {
    def equal[A](p1: Parser[A], p2: Parser[A])(in: Gen[String]): Prop =
      Prop.forAll(in)(s =>
        run(p1)(s) == run(p2)(s))

    def mapLaw[A](p: Parser[A])(in: Gen[String]): Prop =
      equal(p, p.map((a: A) => a))(in)

    //def associativeLaw[A](p1: Parser[A], p2: Parser[A]): Prop = ???

    def unitLaw[A](p: Parser[A])(in: Gen[String]): Prop =
      Prop.forAll(in)(s =>
        run(succeed(s))(s) == Right(s))
  }

  case class ParserOps[A](p: Parser[A]) {
    def |[B >: A](p2: Parser[B]): Parser[B] = self.or(p, p2)

    def or[B >: A](p2: => Parser[B]): Parser[B] = self.or(p, p2)

    def times[B >: A](n: Int): Parser[List[A]] = self.listOfN(n, p)

    def +[B](pB: Parser[B]): Parser[(A, B)] = self.product(p, pB)

    def map[B](f: A => B): Parser[B] = self.map(p)(f)

    def slice(): Parser[String] = self.slice(p)

    def many(): Parser[List[A]] = self.many(p)

    //    def succeed[A](a: A): Parser[A] =
  }
}

//trait Parsers2[ParseError, Parser[+_]] {
//  def or[A](s1: Parser[A], s2: Parser[A]): Parser[A]
//  implicit def string(s: String): Parser[String]
//  implicit def operators[A](p: Parser[A]) = ParserOps[A](p)
//  implicit def asStringParser[A](a: A)(implicit f: A => Parser[String]):
//  ParserOps[String] = ParserOps(f(a))
//
//  case class ParserOps[A](p: Parser[A]) {
//    def |[B>:A](p2: Parser[B]): Parser[B] = self.or(p,p2)
//    def or[B>:A](p2: => Parser[B]): Parser[B] = self.or(p,p2)
//  }
//}