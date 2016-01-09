package scratchpad.mapmergemonoid

import scratchpad.mapmergemonoid.MapMergeMonoid.mapMergeMonoid
import scratchpad.monoid.Monoid

object Main {
  def main(args: Array[String]) = {
    val mapMergeMerge: Monoid[Map[String, Map[String, Int]]] = mapMergeMonoid(mapMergeMonoid(Monoid.intAddition))
    val map1 = Map("o1" -> Map("i1" -> 1, "i2" -> 2))
    val map2 = Map("o1" -> Map("i1" -> 1, "i3" -> 5))
    println(mapMergeMerge.op(map1, map2))


    val indexed = IndexedSeq(1, 2, 4, 2, 3, 1, 1)
    println(MapMergeMonoid.bag(indexed))
  }
}

object MapMergeMonoid {
  def mapMergeMonoid[K, V](V: Monoid[V]): Monoid[Map[K, V]] = new Monoid[Map[K, V]] {
    def zero = Map[K,V]()

    def op(a: Map[K, V], b: Map[K, V]) =
      (a.keySet ++ b.keySet).foldLeft(zero) {
        (acc, k) =>
          acc.updated(k, V.op(a.getOrElse(k, V.zero), b.getOrElse(k, V.zero)))
      }
  }

  def functionMonoid[A,B](b: Monoid[B]): Monoid[A => B] = {
    new Monoid[A => B] {
      override def op(a1: A => B, a2: A => B): A => B =
        (a: A) =>
          b.op(a1(a), a2(a))

      override def zero: A => B =
        (a: A) => b.zero
    }
  }

  def bag[A](as: IndexedSeq[A]): Map[A, Int] = {
    Monoid.foldMapV(as, mapMergeMonoid[A, Int](Monoid.intAddition))(v => Map((v -> 1)))
  }
}