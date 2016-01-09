package scratchpad.wordcountmonoid

import scratchpad.monoid.Monoid

object Main{
  def main(args: Array[String]): Unit = {
    println(WordCountMonoid.countWords("this is a kitty cat inside the computer"))
  }
}

object WordCountMonoid {
  sealed trait WC
  case class Stub(chars: String) extends WC
  case class Part(lStub: String, words: Int, rStub: String) extends WC

  val wcMonoid: Monoid[WC] = new Monoid[WC] {
    override def op(a1: WC, a2: WC): WC = {
      (a1, a2) match {
        case (Stub(a), Stub(b)) => Stub(a + b)
        case (Stub(a), Part(lStubB, lWordsB, rStubB)) => Part(a + lStubB, lWordsB, rStubB)
        case (Part(lStubA, lWordsA, rStubA), Stub(b)) => Part(lStubA, lWordsA, rStubA + b)
        case (Part(lStubA, lWordsA, rStubA), Part(lStubB, lWordsB, rStubB)) => {
          if (rStubA.length > 0 || lStubB.length > 0) {
            Part(lStubA, lWordsA + lWordsB + 1, rStubB)
          } else {
            Part(lStubA, lWordsA + lWordsB, rStubB)
          }
        }
      }
    }

    override def zero: WC = {
      Stub("")
    }
  }

  def countWords(str: String): Int = {
    val result: WC = Monoid.foldMapV(str.toCharArray, wcMonoid)((c: Char) => {
      if (c == ' ') {
        Part("", 0, "")
      } else {
        Stub(c + "")
      }
    })
    result match {
      case Stub(i) if i.length > 0 => 1
      case Stub(_) => 0
      case Part(l, w, r) => {
        w + (if (l.length > 0) 1 else 0) + (if (r.length > 0) 1 else 0)
      }
    }

  }

}
