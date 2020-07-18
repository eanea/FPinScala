package Part1.Chapter5

import Part1.Chapter5.Stream.{Cons, Empty}

sealed trait Stream[+A] {
  def headOption: Option[A] = this match {
    case Empty      => None
    case Cons(h, t) => Some(h())
  }

  /**
    * EXERCISE 5.1
    * @return
    */
  def toList: List[A] = this match {
    case Empty      => Nil
    case Cons(h, t) => h() :: t().toList
  }

  /**
    * EXERCISE 5.2
    * @param n
    * @return
    */
  def take(n: Int): Stream[A] = {
    this match {
      case Empty => Empty
      case Cons(h, t) =>
        if (n != 0) Cons(h, () => t().take(n - 1))
        else Empty
    }
  }

  def drop(n: Int): Stream[A] = {
    this match {
      case Empty => Empty
      case s @ Cons(h, t) =>
        if (n != 0) t().drop(n - 1)
        else s
    }
  }

  /**
    * EXERCISE 5.3
    * @param p
    * @return
    */
  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Empty => Empty
    case Cons(h, t) =>
      if (p(h())) Cons(h, () => t().takeWhile(p))
      else Empty
  }

  def exists(p: A => Boolean): Boolean = this match {
    case Cons(h, t) => p(h()) || t().exists(p)
    case _          => false
  }

  def foldRight[B](z: => B)(f: (A, => B) => B): B = this match {
    case Cons(h, t) => f(h(), t().foldRight(z)(f))
    case _          => z
  }

  /**
    * Not stack-safe exists
    * @param p
    * @return
    */
  def existsViaFoldRight(p: A => Boolean): Boolean =
    foldRight(false)((a, b) => p(a) || b)

  /**
    * EXERCISE 5.4
    * @param p
    * @return
    */
  def forAll(p: A => Boolean): Boolean = foldRight(true)((a, b) => p(a) && b)

  /**
    * EXERCISE 5.5
    */
  def takeWhileViaFoldRight(p: A => Boolean): Stream[A] =
    foldRight(Empty: Stream[A])((a, b) =>
      if (p(a)) Cons(() => a, () => b)
      else Empty
    )

  /**
    * EXERCISE 5.6
    */
  def headOptionViaFoldRight: Option[A] =
    foldRight(None: Option[A])((a, b) => Some(a))

  /**
    * EXERCISE 5.7
    */
  def map[B](f: A => B): Stream[B] =
    foldRight(Empty: Stream[B])((a, b) => Cons(() => f(a), () => b))
  def filter(p: A => Boolean): Stream[A] =
    foldRight(Empty: Stream[A])((a, b) =>
      if (p(a)) Cons(() => a, () => b)
      else b
    )
  def flatMap[B](f: A => Stream[B]): Stream[B] =
    foldRight(Empty: Stream[B])((a, b) => f(a) append b)

  def append[AA >: A](sa: => Stream[AA]): Stream[AA] = this match {
    case Empty      => sa
    case Cons(h, t) => Cons(h, () => t().append(sa))
  }

  /**
    * EXERCISE 5.13
    */
  def mapViaUnfold[B](f: A => B): Stream[B] = Stream.unfold(this) {
    case Empty      => None
    case Cons(h, t) => Some(f(h()), t())
  }

  def takeViaUnfold(n: Int): Stream[A] = Stream.unfold((this, n))(s =>
    if (s._2 == 0) None
    else
      s._1 match {
        case Empty      => None
        case Cons(h, t) => Some(h(), (t(), s._2 - 1))
      }
  )

  def takeWhileViaUnfold(p: A => Boolean): Stream[A] = Stream.unfold(this) {
    case Cons(h, t) if p(h()) => Some(h(), t())
    case _                    => None
  }

  def zipWithViaUnfold[B](sb: Stream[B]): Stream[(A, B)] =
    Stream.unfold((this, sb)) {
      case (Cons(ha, ta), Cons(hb, tb)) => Some((ha(), hb()), (ta(), tb()))
      case _                            => None
    }

  def zipAllViaUnfold[B](sb: Stream[B]): Stream[(Option[A], Option[B])] =
    Stream.unfold((this, sb)) {
      case (Cons(ha, ta), Cons(hb, tb)) => Some((Some(ha()), Some(hb())), (ta(), tb()))
      case (Cons(ha, ta), Empty)        => Some((Some(ha()), None), (ta(), Empty))
      case (Empty, Cons(hb, tb))        => Some((None, Some(hb())), (Empty, tb()))
      case _                            => None
    }

  /**
   * EXERCISE 5.14
   */
  def startsWith[AA >: A](s: Stream[AA]): Boolean = this.zipWithViaUnfold(s).forAll(x => x._1 == x._2)

  /**
   * EXERCISE 5.15
   */
  def tails: Stream[Stream[A]] = Stream.unfold((this, true)) {s =>
    if(s._2) s._1 match {
      case Empty => Some(Stream(), (Empty, false))
      case st @ Cons(h, t) => Some(st, (t(), true))
    } else None
  }

  def hasSubsequence[AA >: A](s: Stream[AA]): Boolean =
    tails exists (_ startsWith s)


  /**
   * EXERCISE 5.16
   */
  def scanRight[B](z: B)(f: (A, => B) => B): Stream[B] = tails.map(_.foldRight(z)(f))

}

object Stream {

  case object Empty                                   extends Stream[Nothing]
  case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

  /**
    * EXERCISE 5.8
    * @param a
    * @tparam A
    * @return
    */
  def constant[A](a: A): Stream[A] = {
    lazy val s: Stream[A] = Stream.cons(a, s)
    s
  }

  /**
    * EXERCISE 5.9
    * @return
    */
  def from(n: Int): Stream[Int] = {
    Stream.cons(n, from(n + 1))
  }

  /**
    * EXERCISE 5.10
    * @return
    */
  private def fibs_(a: => Long, b: => Long): Stream[Long] = Stream.cons(a, fibs_(b, a + b))

  def fibs: Stream[Long] = fibs_(0L, 1L)

  /**
    * EXERCISE 5.11
    * @return
    */
  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = f(z) match {
    case Some((a, s)) => Stream.cons(a, unfold(s)(f))
    case None         => Empty
  }

  /**
    * EXERCISE 5.12
    * @return
    */
  def fibsViaUnfold: Stream[Long]           = unfold((0L, 1L))(s => Some(s._1, (s._2, s._1 + s._2)))
  def fromViaUnfold(n: Int): Stream[Int]    = unfold(n)(s => Some(s, s + 1))
  def constantViaUnfold[A](a: A): Stream[A] = unfold(a)(s => Some(s, s))
  def onesViaUnfold: Stream[Int]            = constantViaUnfold(1)

  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }
  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))

}
