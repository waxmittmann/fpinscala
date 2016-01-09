package scratchpad.done

import fpinscala.monads.Functor
import scratchpad.monoid.Monoid

/*
class Validator {
//  val validationApplicative = new ValidationApplicative[String]
//  import validationApplicative.Ops

  def validName(name: String): Validation[String, String] =
    if (name != "") Success(name)
    else Failure("Name cannot be empty")

  def validBirthdate(birthdate: String): Validation[String, Date] = {
    try {
      import java.text._
      Success((new SimpleDateFormat("yyyy-MM-dd")).parse(birthdate))
    } catch {
      case ex => Failure("Birthdate must be in the form yyyy-MM-dd")
    }
  }

  def validPhone(phoneNumber: String): Validation[String, String] =
    if (phoneNumber.matches("[0-9]{10}"))
      Success(phoneNumber)
    else
      Failure("Phone number must be 10 digits")
}*/

trait Applicative[F[_]] extends Functor[F] { self =>
  //Base
  def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C]

  def unit[A](a: => A): F[A]

  //def apply[A, B](fab: F[A => B])(fa: F[A]): F[B]
  def apply[A, B](fab: F[A => B])(fa: F[A]): F[B]

  //Derived
  def map[A, B](fa: F[A])(f: A => B): F[B] =
    map2(fa, unit(()))((a, _) => f(a))

  def traverse[A, B](as: List[A])(f: A => F[B]): F[List[B]] =
    as.foldRight(unit(List[B]()))((a, fbs) => map2(f(a), fbs)(_ :: _))

  def sequence[A](fas: List[F[A]]): F[List[A]] = traverse(fas)(a => a)

  type Const[A, B] = A
  implicit def monoidApplicative[M](M: Monoid[M]) = new Applicative[({ type f[x] = Const[M, x] })#f] {
    def unit[A](a: => A): M = M.zero
    def map2[A,B,C](m1: M, m2: M)(f: (A,B) => C): M = M.op(m1,m2)
    override def apply[A, B](fab: Const[M, (A) => B])(fa: Const[M, A]): Const[M, B] = ???
  }

  //Traversable
  def sequenceMap[K,V](ofa: Map[K,F[V]]): F[Map[K,V]] = {
    ofa.foldLeft(unit(Map[K,V]()))((mapr: F[Map[K, V]], cur: (K, F[V])) => {
      map2[Map[K, V], V, Map[K, V]](mapr, cur._2)((maprv: Map[K, V], curv: V) => {
        maprv + (cur._1 -> curv)
      })
    })
  }

  def replicateM[A](n: Int, fa: F[A]): F[List[A]] = sequence(List.fill(n)(fa))

  def product[A, B](fa: F[A], fb: F[B]): F[(A, B)] = map2(fa, fb)((_, _))

  def map3[A, B, C, D](fa: F[A], fb: F[B], fc: F[C])(f: (A, B, C) => D): F[D] = {
    val mappingChain: F[(A) => (B) => (C) => D] = unit((a: A) => (b: B) => (c: C) => f(a, b, c))
    apply(apply(apply(mappingChain)(fa))(fb))(fc)
  }

  //Alternates
  def map2ViaApply[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] = {
    val aToBToAB: F[(A) => (B) => (A, B)]
    = unit((a: A) => (b: B) => (a, b))
    val bToAB: F[(B) => (A, B)] = apply(aToBToAB)(fa)
    val fab: F[(A, B)] = apply(bToAB)(fb)
    apply[(A, B), C](unit(f.tupled))(fab)
  }

  def applyViaMap2[A, B](fab: F[A => B])(fa: F[A]): F[B] = {
    map2(fa, fab)((a: A, fAtoB: A => B) => fAtoB(a))
  }

  def product2[G[_]](G: Applicative[G]): Applicative[({type f[x] = (F[x], G[x])})#f] = {
    val self = this
    new Applicative[({type f[x] = (F[x], G[x])})#f] {
      override def unit[A](a: => A) = (self.unit(a), G.unit(a))

      override def apply[A,B](fs: (F[A => B], G[A => B]))(p: (F[A], G[A])) =
        (self.apply(fs._1)(p._1), G.apply(fs._2)(p._2))

      override def map2[A, B, C](fa: (F[A], G[A]), fb: (F[B], G[B]))(f: (A, B) => C) = ???

      //def apply[A, B](fab: F[A => B])(fa: F[A]): F[B]
//      override def apply[A, B](fab: (F[(A) => B], G[(A) => B]))(fa: (F[A], G[A])): (F[B], G[B]) =
//        (self.apply(fab._1)(fa._1), G.apply(fab._2)(fa._2))
    }
  }

  def compose[G[_]](G: Applicative[G]): Applicative[({type f[x] = F[G[x]]})#f] = {
    val F = this
    new Applicative[({type f[x] = F[G[x]]})#f] {
      //Base
      override def map[A, B](fa: F[G[A]])(f: (A) => B): F[G[B]] = ???

      //def apply[A, B](fab: F[A => B])(fa: F[A]): F[B]

      override def apply[A, B](fab: F[G[(A) => B]])(fa: F[G[A]]): F[G[B]] = ???

      override def unit[A](a: => A): F[G[A]] = {
        F.unit(G.unit(a))
      }

      override def map2[A, B, C](fga: F[G[A]], fgb: F[G[B]])(f: (A, B) => C): F[G[C]] = {
        F.map2(fga, fgb)((ga: G[A], gb: G[B]) => {
          G.map2(ga, gb)(f)
        })
      }

    }
  }



  /*
  //Applicative[(F[_], G[_])]
  def product[G[_]](G: Applicative[G]): Applicative[({type f[x] = (F[x], G[x])})#f] = {
    val F : Applicative[F] = this

    type H[_] = (F[_], G[_])
    val FG = new Applicative[({type f[x] = (F[x], G[x])})#f] {
    //val FG = new Applicative[H] {
      //Base
      override def map2[A, B, C](fa: (F[A], G[A]), fb: (F[B], G[B]))(f: (A, B) => C) = { //: H[C] = {
        (F.map2(fa._1, fb._1)(f), G.map2(fa._2, fb._2)(f))
      }

//      override def apply[A, B](fab: (F[A => B], G[A => B]))(fa: (F[A], G[A])): H[B] = {
//        val r: (F[B], G[B]) = (F.apply(fab._1)(fa._1), G.apply(fab._2)(fa._2))
//        r
//      }

      override def apply[A,B](fs: (F[A => B], G[A => B]))(p: (F[A], G[A])) =
        (self.apply(fs._1)(p._1), G.apply(fs._2)(p._2))


      override def unit[A](a: => A): H[A] = (F.unit(a), G.unit(a))
    }
    //unit((this, G))
  }*/
}

/*
sealed trait Validation[+E, +A]

case class Failure[E](head: E, tail: Vector[E] = Vector()) extends Validation[E, Nothing] {
//  def apply[A](a: A): Success[A] = Success(a)
}

case class Success[A](a: A) extends Validation[Nothing, A] {
//  def apply[E](head: E): Failure[E] = Failure(head)
}

//object Success {
//  def apply[A](a: A): Success[A] = Success(a)
//}
//
//object Failure {
//  def apply[E](head: E): Failure[E] = Failure(head)
//}



class ValidationApplicative[E] extends Applicative[({type f[x] = Validation[E, x]})#f] {
  self =>

  //Ops
  implicit class Ops[A](lhs: Validation[E, A]) {
    def map2[B, C](rhs: Validation[E, B])(f: (A, B) => C) = self.map2(lhs, rhs)(f)

//    def apply[B](fab: Validation[E, (A) => B]) = self.apply(fab)(lhs)
  }

  //Base
  override def map2[A, B, C](fa: Validation[E, A], fb: Validation[E, B])(f: (A, B) => C): Validation[E, C] = {
    fa match {
      case Success(valueA) => {
        fb match {
          case Success(valueB) => Success(f(valueA, valueB))
          case Failure(head, tail) => Failure(head, tail)
        }
      }
      case Failure(headA: E, tailA: Vector[E]) => {
        fb match {
          case Success(valueA) => Failure(headA, tailA)
          case Failure(headB: E, tailB: Vector[E]) => mergeErrors(headA, tailA, headB, tailB)
        }
      }
    }
  }

  override def apply[A, B](fab: Validation[E, (A) => B])(fa: Validation[E, A]): Validation[E, B] = {
    fa match {
      case Success(a) => {
        fab match {
          case Success(ab) => Success(ab(a))
          case Failure(headB: E, tailB: Vector[E]) => Failure(headB, tailB)
        }
      }
      case Failure(headA: E, tailA: Vector[E]) => {
        fab match {
          case Success(valueA) => Failure(headA, tailA)
          case Failure(headB: E, tailB: Vector[E]) => mergeErrors(headA, tailA, headB, tailB)
        }
      }
    }
  }

  def mergeErrors[C, B, A](headA: E, tailA: Vector[E], headB: E, tailB: Vector[E]): Failure[E] = {
    Failure(headA, (tailA :+ headB) ++ tailB)
  }

  override def unit[A](a: => A): Validation[E, A] = {
    Success(a)
  }
}*/