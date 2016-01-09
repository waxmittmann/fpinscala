package scratchpad.monoid

import fpinscala.state.RNG
import fpinscala.testing.Prop._
import fpinscala.testing.{Gen, Prop}

object Main {
  def main(args: Array[String]): Unit = {
//    println("Try it")
//    val result: Result = Monoid.monoidLaws(Monoid.intAddition, Gen.smallInt).run(100, 100, RNG.Simple(123))
//    result match {
//      case Passed => println("Succeeded")
//      case Falsified(failCase, _) => println("Failed: " + failCase)
//    }

    val orderedList: IndexedSeq[Int] = IndexedSeq(1, 2, 2, 3, 4, 5, 6, 7, 10)
    println(Monoid.isOrdered(orderedList))

    val unorderedList = IndexedSeq(1, 2, 2, 3, 5, 4, 6, 7, 10)
    println(Monoid.isOrdered(unorderedList))
  }
}

object Monoid {
  val intAddition: Monoid[Int] = new Monoid[Int] {
    def op(a1: Int, a2: Int): Int = {
      a1 + a2
    }

    def zero: Int = 0
  }

  val intMultiplication: Monoid[Int] = new Monoid[Int] {
    def op(a1: Int, a2: Int): Int = {
      a1 * a2
    }

    def zero: Int = 1
  }

  val booleanOr: Monoid[Boolean] = new Monoid[Boolean] {
    def op(a1: Boolean, a2: Boolean): Boolean = {
      a1 || a2
    }

    def zero: Boolean = false
  }

  val booleanAnd: Monoid[Boolean] = new Monoid[Boolean] {
    def op(a1: Boolean, a2: Boolean): Boolean = {
      a1 && a2
    }

    def zero: Boolean = true
  }

  def optionMonoid[A]: Monoid[Option[A]] = new Monoid[Option[A]]{
    def op(a1: Option[A], a2: Option[A]): Option[A] = {
      a1 orElse a2
    }

    def zero: Option[A] = None
  }

  def endoMonoid[A]: Monoid[A => A] = new Monoid[A => A] {
    override def op(a1: A => A, a2: A => A): A => A = {
      (a: A) => a2(a1(a))
    }

    override def zero: (A) => A = a => a
  }

  def productMonoid[A,B](a: Monoid[A], b: Monoid[B]): Monoid[(A,B)] = {
    new Monoid[(A, B)] {
      override def op(a1: (A, B), a2: (A, B)): (A, B) =
        (a.op(a1._1, a2._1), b.op(a1._2, a2._2))

      override def zero: (A, B) =
        (a.zero, b.zero)
    }
  }

  def foldMap[A,B](as: List[A], m: Monoid[B])(f: A => B): B = {
    as.foldLeft(m.zero)((coll: B, cur: A) => {
      m.op(coll, f(cur))
    })
  }

  def foldRight[A, B](as: List[A])(z: B)(f: (A, B) => B): B = {
    val curried: (A) => (B) => B = f.curried
    val endo: Monoid[B => B] = endoMonoid[B]
    foldMap(as, endo)(curried)(z)
  }

  // We can get the dual of any monoid just by flipping the `op`.
  def dual[A](m: Monoid[A]): Monoid[A] = new Monoid[A] {
    def op(x: A, y: A): A = m.op(y, x)

    val zero = m.zero
  }

  def foldLeft[A, B](list: List[A])(z: B)(f: (A, B) => B): B = {
    val curried: (A) => (B) => B = f.curried
    val flippedEndo: Monoid[B => B] = dual(endoMonoid[B])
    foldMap(list, flippedEndo)(curried)(z)
  }

  def foldMapV[A,B](v: IndexedSeq[A], m: Monoid[B])(f: A => B): B = {
    if (v.size == 0) {
      m.zero
    } else if (v.size == 1) {
     f(v(0))
    } else {
      val (v1, v2) = v.splitAt(v.length / 2)
      m.op(foldMapV(v1, m)(f), foldMapV(v2, m)(f))
    }
  }

  type IsOrderedResult = Either[String, (Int, Int)]

  def isOrdered(v: IndexedSeq[Int]): IsOrderedResult = {
    val orderedMonoid = new Monoid[IsOrderedResult] {
      override def op(a1: IsOrderedResult , a2: IsOrderedResult): IsOrderedResult = {
        for {
          range1 <- a1.right
          range2 <- a2.right
          result <- {
            val firstSegmentEnd: Int = range1._2
            val secondSegmentStart: Int = range2._1
            if (firstSegmentEnd <= secondSegmentStart)
              Right[String, (Int, Int)](range1._1, range2._2)
            else
              Left[String, (Int, Int)](firstSegmentEnd + " >= " + secondSegmentStart)
          }.right
        } yield result
      }

      override def zero: IsOrderedResult = Right((-1, -1))
    }

    foldMapV(v, orderedMonoid)(a => Right(a, a))
  }

  import fpinscala.parallelism.Nonblocking._

  def par[A](m: Monoid[A]): Monoid[Par[A]] = new Monoid[Par[A]] {
    override def op(a1: Par[A], a2: Par[A]): Par[A] = {
      Par.map2(a1, a2)((a1v, a2v) => m.op(a1v, a2v))
    }

    override def zero: Par[A] = Par.unit(m.zero)
  }


  //Official version
//  def parFoldMap[A,B](v: IndexedSeq[A], m: Monoid[B])(f: A => B): Par[B] =
//    Par.parMap(v)(f).flatMap { bs =>
//      foldMapV(bs, par(m))(b => Par.async(b))
//    }

  //I think this works too?
  def parFoldMap[A,B](v: IndexedSeq[A], m: Monoid[B])(f: A => B): Par[B] = {
    def go[A,B](v: IndexedSeq[A], m: Monoid[Par[B]])(f: A => B): Par[B] = {
      if (v.size == 0) {
        m.zero
      } else if (v.size == 1) {
        Par.delay(f(v(0)))
      } else {
        val (v1, v2) = v.splitAt(v.length / 2)
        m.op(go(v1, m)(f), go(v2, m)(f))
      }
    }
    go(v, par(m))(f)
  }



def monoidLaws[A](m: Monoid[A], gen: Gen[A]): Prop = {
    val associativityProp = Prop.forAll(gen.listOfN(2))((s: List[A]) => {
      val (a1, a2) = (s(0), s(1))
      m.op(a1, a2) == m.op(a2, a1)
    })

    val zeroProp = Prop.forAll(gen)((a: A) => {
      m.op(a, m.zero) == a
    })

    associativityProp && zeroProp
  }
}

trait Monoid[A] {
  def op(a1: A, a2: A): A
  def zero: A
}