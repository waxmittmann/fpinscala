package scratchpad.foldable

import scratchpad.monoid.Monoid

import scratchpad.monoid._

object Main {
  def main(args: Array[String]): Unit = {
    val strings = List("a", "bc", "d", "efg", "h")
    println(ListFoldable.foldRight(strings)("")((cur: String, str: String) => cur + str))
  }
}

trait Foldable[F[_]] {
  def foldRight[A, B](as: F[A])(z: B)(f: (A, B) => B): B

  def foldLeft[A, B](as: F[A])(z: B)(f: (B, A) => B): B

  def foldMap[A, B](as: F[A])(f: A => B)(mb: Monoid[B]): B

  def toList[A](fa: F[A]): List[A] = {
    foldRight(fa)(List.empty[A])((cur: A, li: List[A]) => cur :: li)
  }

//  def concatenate[A](as: F[A])(m: Monoid[A]): A
}

object ListFoldable extends Foldable[List] {
  override def foldRight[A, B](as: List[A])(z: B)(f: (A, B) => B) =
    as.foldRight(z)(f)
  override def foldLeft[A, B](as: List[A])(z: B)(f: (B, A) => B) =
    as.foldLeft(z)(f)
  override def foldMap[A, B](as: List[A])(f: A => B)(mb: Monoid[B]): B =
    foldLeft(as)(mb.zero)((b, a) => mb.op(b, f(a)))
}

object OptionFoldable extends Foldable[Option] {
  override def foldRight[A, B](as: Option[A])(z: B)(f: (A, B) => B): B = {
    as.fold(z)(f(_, z))
  }

  override def foldLeft[A, B](as: Option[A])(z: B)(f: (B, A) => B): B = {
    as.fold(z)(f(z, _))
  }

  override def foldMap[A, B](as: Option[A])(f: (A) => B)(mb: Monoid[B]): B = {
    as.fold(mb.zero)(a => f(a))
  }
}

object IndexedSeqFoldable extends Foldable[IndexedSeq] {
  import Monoid._
  override def foldRight[A, B](as: IndexedSeq[A])(z: B)(f: (A, B) => B) =
    as.foldRight(z)(f)
  override def foldLeft[A, B](as: IndexedSeq[A])(z: B)(f: (B, A) => B) =
    as.foldLeft(z)(f)
  override def foldMap[A, B](as: IndexedSeq[A])(f: A => B)(mb: Monoid[B]): B =
    foldMapV(as, mb)(f)
}

//object StreamFoldable extends Foldable[Stream] {
//  override def foldRight[A, B](as: Stream[A])(z: B)(f: (A, B) => B) =
//    as.foldRight(z)(f)
//  override def foldLeft[A, B](as: Stream[A])(z: B)(f: (B, A) => B) =
//    as.foldLeft(z)(f)
//}
