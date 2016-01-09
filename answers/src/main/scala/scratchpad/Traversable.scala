package scratchpad.traversable

import fpinscala.applicative.Applicative
import fpinscala.monads.Functor
import fpinscala.monoids.{Monoid, Foldable}

trait Traverse[F[_]] extends Functor[F] with Foldable[F] {
  self =>
  def traverse[G[_] : Applicative, A, B](fa: F[A])(f: A => G[B]): G[F[B]]

  def unit[A](a: A): F[A]

  //sequence(map(fa)(f))

  def sequence[G[_] : Applicative, A](fga: F[G[A]]): G[F[A]] =
    traverse(fga)(ga => ga)

  //traverse map via traverse
  def tmap[A, B](fa: F[A])(f: A => B): F[B] = {
    case class Container[B](b: B)

    val easyApplicative = new Applicative[Container] {
      override def unit[A](a: => A): Container[A] = Container(a)
      override def map2[A,B,C](fa: Container[A], fb: Container[B])(f: (A, B) => C): Container[C] = {
        Container(f(fa.b, fb.b))
      }
    }

    traverse[Container, A, B](fa)((a: A) => easyApplicative.unit(f(a)))(easyApplicative).b
  }

}

//class ListTraverse[A] extends Traverse[List[A]] {

object Main {
  def main(args: Array[String]) = {
//    val li = List(1, 2, 3, 4, 5)
    val li = List(2, 4, 8, 16)
    val f: Int => Option[String] = (i: Int) => //Some(i.toString)
      if (i % 2 == 0)
        Some("<" + i + ">")
      else
        None

    val optionApplicative = new Applicative[Option] {
      override def unit[A](a: => A): Option[A] = Some(a)

      override def map2[A,B,C](fa: Option[A], fb: Option[B])(f: (A, B) => C): Option[C] =
        for {
          a <- fa
          b <- fb
        } yield f(a, b)
    }
    println(listTraverse.traverse(li)(f)(optionApplicative))

    val listApplicative = new Applicative[List] {
      override def unit[A](a: => A): List[A] = {
        List(a)
      }

      override def map2[A,B,C](fa: List[A], fb: List[B])(f: (A, B) => C): List[C] = {
        fa.zip(fb).map(f.tupled)
      }
    }
    val f2: Int => List[Int] = (i: Int) => //Some(i.toString)
      if (i % 2 == 0)
        List(i)
      else
        Nil
    println(listTraverse.traverse(li)(f2)(listApplicative))


    println(listTraverse.tmap(li)(_ + 1))

  }

  val listTraverse = new Traverse[List] {
    override def traverse[G[_],A,B](as: List[A])(f: A => G[B])(implicit G: Applicative[G]): G[List[B]] = {
      println("Inside traverse")
      as.foldRight(G.unit(List[B]()))((a, fbs) => G.map2(f(a), fbs)(_ :: _))
    }

    //extends Functor[F] with Foldable[F] { self =>
//    override def traverse[G[_], A, B](fa: List[A])(f: (A) => G[B])(implicit G: Applicative[G]): G[List[B]] = {
//      fa.foldRight(G.unit(List[B]()))((a: A, li: G[List[B]]) => {
//        G.map2(li, f(a))((li: List[B], b: B) => {
//          b :: li
//        })
//      })
//    }

    override def map[A, B](fa: List[A])(f: (A) => B): List[B] = ???

    override def unit[A](a: A): List[A] = List(a)
  }

  /*
  trait Functor[F[_]] {
    def map[A,B](fa: F[A])(f: A => B): F[B]

    def distribute[A,B](fab: F[(A, B)]): (F[A], F[B]) =
      (map(fab)(_._1), map(fab)(_._2))

    def codistribute[A,B](e: Either[F[A], F[B]]): F[Either[A, B]] = e match {
      case Left(fa) => map(fa)(Left(_))
      case Right(fb) => map(fb)(Right(_))
    }
  }
   */


  /*
  trait Foldable[F[_]] {
  import Monoid._

  def foldRight[A,B](as: F[A])(z: B)(f: (A, B) => B): B =
    foldMap(as)(f.curried)(endoMonoid[B])(z)

  def foldLeft[A,B](as: F[A])(z: B)(f: (B, A) => B): B =
    foldMap(as)(a => (b: B) => f(b, a))(dual(endoMonoid[B]))(z)

  def foldMap[A, B](as: F[A])(f: A => B)(mb: Monoid[B]): B =
    foldRight(as)(mb.zero)((a, b) => mb.op(f(a), b))

  def concatenate[A](as: F[A])(m: Monoid[A]): A =
    foldLeft(as)(m.zero)(m.op)

  def toList[A](as: F[A]): List[A] =
    foldRight(as)(List[A]())(_ :: _)
}
   */
}
