package scratchpad.done

import scratchpad.done.Applicative

object Main {
  def main(args: Array[String]): Unit = {
    println(ListMonad.replicateM(10, List[Int](1, 2, 3)))
    println(OptionMonad.replicateM(5, Some("a")))
    println(ListMonad.filterM(List(1, 2, 3, 4, 5, 6))((ele: Int) => List[Boolean](ele % 2 == 0)))
    println(OptionMonad.filterM(List(Some(1), Some(2), Some(3), Some(4)))((ele: Option[Int]) => {
      ele.flatMap((eleVal) => {
        if (eleVal % 2 == 0) {
          Some(true)
        } else {
          Some(false)
        }})
    }))

    val ei: Either[String, Int] = Right(1)
    val eitherMonad = new EitherMonad[String]

    println(eitherMonad.flatMap(ei)((a: Int) => if (a == 1) Left("Crazy") else Right(2)))
    println(eitherMonad.apply(Left("BadBad"))(ei))
    println(eitherMonad.apply(Right((a: Int) => a + 1))(ei))
  }
}

trait Monad[F[_]] { self =>
//trait Monad[F[_]] extends Applicative[F] { self =>
  implicit class Operations[A](fa: F[A]) {
    def map[B](f: A => B): F[B] = self.map(fa)(f)
    def flatMap[B](f: A => F[B]): F[B] = self.flatMap(fa)(f)
  }

  //Primitives
  def unit[A](a: A): F[A]

  def flatMap[A,B](fa: F[A])(f: A => F[B]): F[B]

  //Compounds
  def map2[A,B,C](fa: F[A], fb: F[B])(f: (A,B) => C): F[C] = {
    fa flatMap (a => fb map (b => f(a, b)))
  }

  def apply[A, B](fab: F[(A) => B])(fa: F[A]): F[B]

  //These now come from Applicative
  def map[A,B](fa: F[A])(f: A => B): F[B] = {
    fa.flatMap((a: A) => unit(f(a)))
  }

  def sequence[A](lma: List[F[A]]): F[List[A]] = {
    lma.foldRight(unit(List.empty[A]))((cur: F[A], result: F[List[A]]) => {
      cur.flatMap((a: A) => {
        result.map((li: List[A]) => a :: li)
      })
    })
  }

  def traverse[A,B](la: List[A])(f: A => F[B]): F[List[B]] = {
    la.foldRight(unit(List.empty[B]))((cur: A, result: F[List[B]]) => {
      map2(f(cur), result)((_ :: _))
    })
  }

  //The answer is nicer
  def replicateM[A](n: Int, ma: F[A]): F[List[A]] = {
    //to? or until?
    ma.flatMap((a: A) => unit((1 until n).foldLeft(List[A]())((liA: List[A], b) => a :: liA)))
  }

  def product[A,B](ma: F[A], mb: F[B]): F[(A, B)] = map2(ma, mb)((_, _))

  def merge[A](liFA: F[List[A]], a: A): F[List[A]] = {
    liFA.map(li => {
      a :: li
    })
  }

  def filterM[A](ms: List[A])(f: A => F[Boolean]): F[List[A]] = {
    ms.foldRight(unit(List.empty[A]))((a: A, li: F[List[A]]) => {
      f(a).flatMap(if (_) {
        merge(li, a)
      } else {
        li
      })
    })
  }

  def compose[A,B,C](f: A => F[B], g: B => F[C]): A => F[C] = {
    (a: A) => f(a).flatMap(fa => g(fa))
  }

  def flatMapViaCompose[A,B](fa: F[A])(f: A => F[B]): F[B] = {
    compose((_: Unit) => fa, f)()
  }

  def join[A](mma: F[F[A]]): F[A] = mma.flatMap(fa => fa)

  def flatMapViaJoinAndMap[A,B](fa: F[A])(f: A => F[B]): F[B] = join(fa.map(f(_)))
}

object OptionMonad extends Monad[Option] {
  override def unit[A](a: A): Option[A] = Some(a)

  override def flatMap[A, B](fa: Option[A])(f: (A) => Option[B]): Option[B] = {
    val result: Option[B] = fa match {
      case Some(a: A) => f(a)
      case None => None
    }
    result
  }

  override def apply[A, B](fab: Option[(A) => B])(fa: Option[A]): Option[B] = {
    fa.flatMap((a: A) => fab.flatMap((fab: (A => B)) => Some(fab(a))))
  }
}

//object ListMonad {
//  def m[A] = new ListMonad[A]
//}

object ListMonad extends Monad[List] {
  override def unit[A](a: A): List[A] = List(a)

  override def flatMap[A, B](fa: List[A])(f: (A) => List[B]): List[B] = {
    fa.flatMap(f)
  }

  override def apply[A, B](fab: List[(A) => B])(fa: List[A]): List[B] = {
    fa.flatMap((a: A) => {
      fab.foldRight(List[B]())((fa: A => B, li: List[B]) => fa(a) :: li)
    })
  }
}

case class Id[A](value: A) {
  def flatMap[A, B](fa: Id[A])(f: (A) => Id[B]): Id[B] = {
    f(fa.value)
  }

  def map[A, B](fa: Id[A])(f: (A) => B): Id[B] = {
    unit(f(fa.value))
  }

  def unit[A](a: A): Id[A] = {
    Id(a)
  }
}

//def eitherMonad[E]: Monad[({type f[x] = Either[E, x]})#f]

class EitherMonad[E] extends Monad[({type f[x] = Either[E, x]})#f] {
  //Primitives
  override def unit[A](a: A): Either[E, A] = {
    Right(a)
  }

  override def flatMap[A, B](fa: Either[E, A])(f: (A) => Either[E, B]): Either[E, B] = {
    fa.right.flatMap(f(_))
  }

  override def apply[A, B](fab: Either[E, (A) => B])(fa: Either[E, A]): Either[E, B] = {
    fab.right.flatMap((f: A => B) =>
      fa.right.flatMap((a: A) => Right(f(a)))
    )
  }
}

//class StateMonad extends Monad[State] {
//  type AS[A, B] = (A, B)
//
//  //Primitives
//  override def unit[A, B](a: AS): State[A, B] = ???
//
//  override def flatMap[A, B](fa: State[A])(f: (A) => State[B]): State[B] = ???
//}
