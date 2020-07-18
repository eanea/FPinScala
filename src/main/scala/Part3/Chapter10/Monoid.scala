package Part3.Chapter10

import Part2.Chapter7.Par
import Part2.Chapter7.Par.Par
import Part3.Chapter10.Foldable.FoldableIndexedSeq

trait Monoid[A] {
  def op(a1: A, a2: A): A
  def zero: A
}

object Monoid {
  val stringMonoid = new Monoid[String] {
    def op(a1: String, a2: String) = a1 + a2
    val zero                       = ""
  }

  def listMonoid[A] = new Monoid[List[A]] {
    def op(a1: List[A], a2: List[A]) = a1 ++ a2
    val zero                         = Nil
  }

  /**
    * EXERCISE 10.1
    */
  val intAddition: Monoid[Int] = new Monoid[Int] {
    override def op(a1: Int, a2: Int): Int = a1 + a2

    override def zero: Int = 0
  }
  val intMultiplication: Monoid[Int] = new Monoid[Int] {
    override def op(a1: Int, a2: Int): Int = a1 * a2

    override def zero: Int = 1
  }
  val booleanOr: Monoid[Boolean] = new Monoid[Boolean] {
    override def op(a1: Boolean, a2: Boolean): Boolean = a1 || a2

    override def zero: Boolean = false
  }
  val booleanAnd: Monoid[Boolean] = new Monoid[Boolean] {
    override def op(a1: Boolean, a2: Boolean): Boolean = a1 && a2

    override def zero: Boolean = true
  }

  /**
    * EXERCISE 10.2
    */
  def optionMonoid[A]: Monoid[Option[A]] = new Monoid[Option[A]] {
    override def op(a1: Option[A], a2: Option[A]): Option[A] = a1.orElse(a2)

    override def zero: Option[A] = None
  }

  /**
    * EXERCISE 10.3
    */
  def endoMonoid[A]: Monoid[A => A] = new Monoid[A => A] {
    override def op(a1: A => A, a2: A => A): A => A = a => a2(a1(a))

    override def zero: A => A = a => a
  }

  def concatenate[A](as: List[A], m: Monoid[A]): A =
    as.foldLeft(m.zero)(m.op)

  /**
    * EXERCISE 10.5
    */
  def foldMap[A, B](as: List[A], m: Monoid[B])(f: A => B): B = concatenate(as.map(f), m)

  def dual[A](m: Monoid[A]): Monoid[A] = new Monoid[A] {
    def op(x: A, y: A): A = m.op(y, x)
    val zero              = m.zero
  }

  /**
    * EXERCISE 10.6
    */
  def foldRight[A, B](as: List[A])(z: B)(f: (A, B) => B): B = foldMap(as, endoMonoid[B])(a => b => f(a, b))(z)
  def foldLeft[A, B](as: List[A])(z: B)(f: (B, A) => B): B  = foldMap(as, endoMonoid[B])(a => b => f(b, a))(z)

  /**
    * EXERCISE 10.7
    */
  def foldMapV[A, B](v: IndexedSeq[A], m: Monoid[B])(f: A => B): B = if (v.isEmpty) m.zero
  else if (v.length == 1) f(v.head)
  else {
    val len = v.length
    val mid = len / 2
    val h1  = v.slice(0, mid)
    val h2  = v.slice(mid, len)
    m.op(foldMapV(h1, m)(f), foldMapV(h2, m)(f))
  }

  /**
    * EXERCISE 10.8
    */
  def par[A](m: Monoid[A]): Monoid[Par[A]] = new Monoid[Par[A]] {
    override def op(a1: Par[A], a2: Par[A]): Par[A] = Par.map2(a1, a2)(m.op)

    override def zero: Par[A] = Par.unit(m.zero)
  }
  def parFoldMap[A, B](v: IndexedSeq[A], m: Monoid[B])(f: A => B): Par[B] = Par.unit(foldMapV(v, m)(f))
  def parFoldMap2[A, B](v: IndexedSeq[A], m: Monoid[B])(f: A => B): Par[B] =
    Par.flatMap(Par.parMap(v)(f))(l => foldMapV(l, par(m))(b => Par.lazyUnit(b)))

  /**
    * EXERCISE 10.9
    */
  val orderedIntMonoid = new Monoid[Option[(Int, Int, Boolean)]] {
    def op(o1: Option[(Int, Int, Boolean)], o2: Option[(Int, Int, Boolean)]) =
      (o1, o2) match {
        case (Some((min1, max1, b1)), Some((min2, max2, b2))) =>
          Some((min1 min min2, max1 max max2, b1 && b2 && max1 <= min2))
        case (x, None) => x
        case (None, x) => x
      }
    val zero = None
  }

  def ordered(ints: IndexedSeq[Int]): Boolean = foldMapV(ints, orderedIntMonoid)(i => Some((i, i, true))).forall(_._3)

  /**
    * EXERCISE 10.10
    */
  sealed trait WC
  case class Stub(chars: String)                            extends WC
  case class Part(lStub: String, words: Int, rStub: String) extends WC

  val wcMonoid: Monoid[WC] = new Monoid[WC] {
    override def op(a1: WC, a2: WC): WC = (a1, a2) match {
      case (Part(l1, w1, r1), Part(l2, w2, r2)) => Part(l1, w1 + w2 + 1, r2)
      case (Stub(s), Part(l2, w2, r2))          => Part(s + l2, w2, r2)
      case (Part(l1, w1, r1), Stub(s))          => Part(l1, w1, r1 + s)
      case (Stub(s1), Stub(s2))                 => Stub(s1 + s2)
    }

    override def zero: WC = Stub("")
  }

  /**
    * EXERCISE 10.11
    */
  def countWords(s: String, m: Monoid[WC]): Int = {

    def go(str: String, m: Monoid[WC]): WC = {
      if (str.isBlank) Part("", 0, "")
      else if (str.trim.contains(" ")) {
        val (s1, s2) = str.splitAt(str.length / 2)
        m.op(go(s1, m), go(s2, m))
      } else if (str.startsWith(" ") && str.endsWith(" ")) Part("", 1, "")
      else if (str.startsWith(" ")) Part("", 0, str)
      else if (str.endsWith(" ")) Part(str, 0, "")
      else Stub(str)
    }

    go(s, m) match {
      case Stub(chars)               => 1
      case Part(lStub, words, rStub) => words
    }
  }

  /**
    * EXERCISE 10.16
    */
  def productMonoid[A, B](A: Monoid[A], B: Monoid[B]): Monoid[(A, B)] = new Monoid[(A, B)] {
    override def op(a1: (A, B), a2: (A, B)): (A, B) = A.op(a1._1, a2._1) -> B.op(a1._2, a2._2)

    override def zero: (A, B) = A.zero -> B.zero
  }

  def mapMergeMonoid[K, V](V: Monoid[V]): Monoid[Map[K, V]] =
    new Monoid[Map[K, V]] {
      def zero = Map[K, V]()
      def op(a: Map[K, V], b: Map[K, V]) =
        (a.keySet ++ b.keySet).foldLeft(zero) { (acc, k) =>
          acc.updated(k, V.op(a.getOrElse(k, V.zero), b.getOrElse(k, V.zero)))
        }
    }

  /**
   * EXERCISE 10.17
   */
  def functionMonoid[A,B](B: Monoid[B]): Monoid[A => B] = new Monoid[A => B] {
    override def op(a1: A => B, a2: A => B): A => B = a => B.op(a1(a), a2(a))

    override def zero: A => B = a => B.zero
  }

  /**
   * EXERCISE 10.18
   */
  def bag[A](as: IndexedSeq[A]): Map[A, Int] = FoldableIndexedSeq.foldLeft(as)(Map.empty[A, Int]){
    (acc, a) => acc.updated(a, acc.getOrElse(a, 0) + 1)
  }
}
