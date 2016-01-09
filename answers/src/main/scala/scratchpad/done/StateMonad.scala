package scratchpad.statemonad

import fpinscala.monads.Monad
import fpinscala.state.State
import scratchpad.statemonad.StateMonad._

object Main {
  def main(args: Array[String]): Unit = {

    val intStateMonad = stateMonad[Int]
    val intStringState = new IntStringState((i: Int) => ("Number: " + i, i+1))
    val intStringStateB = new IntIntState((i: Int) => (i * i, (i+1)%4))
//    intStateMonad
//    val intStringState2 = IntStateMonad.replicateM(3, intStringState)
    val intStringState2 = intStateMonad.replicateM(3, intStringState)
    println(intStringState.run(2))
    println(intStringState2.run(2))

    val intStringState3: State[Int, String] = intStateMonad.map2(intStringState, intStringStateB)((str, i) => "(" + str + ", and square is: " + i + ")")
    println(intStringState3.run(1))

    println(intStateMonad.sequence(List(intStringState, intStringStateB, intStringState2)).run(1))


    println(getState.run(intStringState.run(1)))
    println(setState(5).run(1000))
    println(setState(5).flatMap(_ => getState).run(1))

      //.flatMap((()) => intStringState3) //.run(intStringState.run(1))

    //intStringState.map(intStringState.run(1), StateMonad.getState)

    {
      val F = stateMonad[Int]

      def zipWithIndex[A](as: List[A]): List[(Int,A)] =
        as.foldLeft(F.unit(List[(Int, A)]()))(f = (acc, a) => for {
          xs <- acc
          n <- getState
          _ <- setState(n + 1)
        } yield (n, a) :: xs).run(0)._1.reverse

      println(zipWithIndex(List("a", "b", "c", "d")))

      def zipWithIndex2[A](as: List[A]): List[(Int,A)] =
        as.foldLeft(F.unit(List[(Int, A)]()))(f = (acc: State[Int, List[(Int, A)]], a: A) =>
          (acc).flatMap { (xs: List[(Int, A)]) =>
              getState.flatMap { (n: Int) =>
                setState(n + 1).map {
                  (_: Unit) => (n, a) :: xs
                }
              }
        }).run(0)._1.reverse

    }

  }
}

object StateMonad {
  type IntState[A] = State[Int, A]
  type IntStringState = State[Int, String]
  type IntIntState = State[Int, Int]

//  def getState[S]: State[S, S] = State((s: S) => (s, s))
  def setState[S](s: => S): State[S, Unit] = State[S, Unit]((_: S) => ((), s))

  def getState[S]: State[S,S] = State(s => (s,s))
//  def setState[S](s: S): State[S,Unit] = State(_ => ((),s))

  def stateMonad[S] = new Monad[({type f[x] = State[S,x]})#f] {
    def unit[A](a: => A): State[S,A] = State(s => (a, s))
    override def flatMap[A,B](st: State[S,A])(f: A => State[S,B]): State[S,B] =
      st flatMap f
  }
}

object IntStateMonad extends Monad[IntState] {
  def unit[A](a: => A): IntState[A] = State(s => (a, s))
  override def flatMap[A,B](st: IntState[A])(f: A => IntState[B]): IntState[B] =
    st flatMap f
}