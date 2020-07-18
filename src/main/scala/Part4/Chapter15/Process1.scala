package Part4.Chapter15

import Part3.Chapter12.Monad
import Part4.Chapter13.IO
import Part4.Chapter15.Process1.{Await, Emit, Halt}

sealed trait Process1[I, O] {
  def apply(s: LazyList[I]): LazyList[O] = this match {
    case Halt() => LazyList()
    case Await(recv) =>
      s match {
        case h #:: t => recv(Some(h))(t)
        case xs      => recv(None)(xs)
      }
    case Emit(h, t) => h #:: t(s)
  }

  def repeat: Process1[I, O] = {
    def go(p: Process1[I, O]): Process1[I, O] = p match {
      case Halt() => go(this)
      case Await(recv) =>
        Await {
          case None => recv(None)
          case i    => go(recv(i))
        }
      case Emit(h, t) => Emit(h, go(t))
    }
    go(this)
  }

  /**
    * EXERCISE 15.5
    */
  def |>[O2](p2: Process1[O, O2]): Process1[I, O2] = p2 match {
    case Emit(o2, poo2) => Emit(o2, this |> poo2)
    case Await(g) =>
      this match {
        case Emit(o, pio) => pio |> g(Some(o))
        case Await(f)     => Await(i => f(i) |> p2)
        case Halt()       => Halt() |> g(None)
      }
    case Halt() => Halt()
  }

  def map[O2](f: O => O2): Process1[I, O2] = this |> Process1.lift(f)

  def ++(p: => Process1[I, O]): Process1[I, O] = this match {
    case Halt()      => p
    case Emit(h, t)  => Emit(h, t ++ p)
    case Await(recv) => Await(recv andThen (_ ++ p))
  }

  def flatMap[O2](f: O => Process1[I, O2]): Process1[I, O2] = this match {
    case Halt()      => Halt()
    case Emit(h, t)  => f(h) ++ t.flatMap(f)
    case Await(recv) => Await(recv andThen (_ flatMap f))
  }

  /**
    * EXERCISE 15.6
    */
  def zipWithIndex: Process1[I, (O, Int)] = {
    def go(p: Process1[I, O], n: Int): Process1[I, (O, Int)] = p match {
      case Emit(head, tail) => Emit((head, n), go(tail, n + 1))
      case Await(recv) =>
        Await {
          case Some(i) => go(recv(Some(i)), n)
          case None    => Halt()
        }
      case Halt() => go(this, n)
    }
    go(this, 0)
  }
}

object Process1 {
  case class Emit[I, O](head: O, tail: Process1[I, O] = Halt[I, O]()) extends Process1[I, O]
  case class Await[I, O](recv: Option[I] => Process1[I, O])           extends Process1[I, O]
  case class Halt[I, O]()                                            extends Process1[I, O]

  def liftOne[I, O](f: I => O): Process1[I, O] =
    Await {
      case Some(i) => Emit(f(i))
      case None    => halt
    }

  def lift[I, O](f: I => O): Process1[I, O] = liftOne(f).repeat

  def emit[I, O](head: O, tail: Process1[I, O] = Halt[I, O]()): Process1[I, O] =
    Emit(head, tail)

  def await[I, O](f: I => Process1[I, O], fallback: Process1[I, O] = Halt[I, O]()): Process1[I, O] =
    Await[I, O] {
      case Some(i) => f(i)
      case None    => fallback
    }

  def halt[I, O] = Halt[I, O]()

  def filter[I](p: I => Boolean): Process1[I, I] =
    Await[I, I] {
      case Some(i) if p(i) => emit(i)
      case _               => halt
    }.repeat

  def sum: Process1[Double, Double] = {
    def go(acc: Double): Process1[Double, Double] =
      Await {
        case Some(d) => Emit(d + acc, go(d + acc))
        case None    => halt
      }
    go(0.0)
  }

  /**
    * EXERCISE 15.1
    */
  def take[I](n: Int): Process1[I, I] = if (n <= 0) Halt() else await(i => emit(i, take(n - 1)))
  def drop[I](n: Int): Process1[I, I] = if (n > 0) await(_ => drop(n - 1)) else lift(i => i)

  def takeWhile[I](f: I => Boolean): Process1[I, I] = await(i =>
    if (f(i)) emit(i, takeWhile(f))
    else halt
  )

  def dropWhile[I](f: I => Boolean): Process1[I, I] = await(i =>
    if (f(i)) dropWhile(f)
    else emit(i, lift(v => v))
  )

  /**
    * EXERCISE 15.2
    */
  def count[I]: Process1[I, Int] = {
    def go(n: Int): Process1[I, Int] = await(i => emit(n + 1, go(n + 1)))
    go(0)
  }

  /**
    * EXERCISE 15.3
    */
  def mean: Process1[Double, Double] = {
    def go(n: Int, m: Double): Process1[Double, Double] = await(i => emit((m + i) / (n + 1), go(n + 1, m + i)))
    go(0, 0)
  }

  def loop[S, I, O](z: S)(f: (I, S) => (O, S)): Process1[I, O] =
    await((i: I) =>
      f(i, z) match {
        case (o, s2) => emit(o, loop(s2)(f))
      }
    )

  /**
    * EXERCISE 15.4
    */
  def sumViaLoop: Process1[Double, Double] = loop(0d)((i, s) => (i + s, s + i))
  def countViaLoop[I]: Process1[I, Int]    = loop(0)((i, s) => (s + 1, s + 1))

  def monad[I]: Monad[Process1[I, *]] =
    new Monad[Process1[I, *]] {
      def unit[O](o: => O): Process1[I, O] = Emit(o)
      def flatMap[O, O2](p: Process1[I, O])(f: O => Process1[I, O2]): Process1[I, O2] =
        p flatMap f
    }

  /**
    * EXERCISE 15.7
    */
  def zip[A, B, C](p1: Process1[A, B], p2: Process1[A, C]): Process1[A, (B, C)] = (p1, p2) match {
    case (Halt(), _)                  => halt
    case (_, Halt())                  => halt
    case (Emit(h1, t1), Emit(h2, t2)) => Emit((h1, h2), zip(t1, t2))
    case (Await(recv), a2 @ _) =>
      Await {
        case s @ Some(_) => zip(recv(s), feed(s)(a2))
        case None        => zip(recv(None), feed(None)(a2))
      }
    case (a1 @ _, Await(recv)) =>
      Await {
        case s @ Some(_) => zip(feed(s)(a1), recv(s))
        case None        => zip(feed(None)(a1), recv(None))
      }
  }

  def feed[A, B](oa: Option[A])(p: Process1[A, B]): Process1[A, B] =
    p match {
      case Halt()      => p
      case Emit(h, t)  => Emit(h, feed(oa)(t))
      case Await(recv) => recv(oa)
    }

  def meanViaZip: Process1[Double, Double] = zip(sum, count[Double]) map (i => i._1 / i._2)

  /**
   * EXERCISE 15.8
   */
  def exists[I](f: I => Boolean): Process1[I, Boolean] = {
    def go(b: Boolean): Process1[I, Boolean] = if (b) emit(true)
    else await(i => go(f(i)), emit(false))
    go(false)
  }

  def processFile[A,B](f: String,
                       p: Process1[String, A],
                       z: B)(g: (B, A) => B): IO[B] = IO {
    @annotation.tailrec
    def go(ss: Iterator[String], cur: Process1[String, A], acc: B): B =
      cur match {
        case Halt() => acc
        case Await(recv) =>
          val next = if (ss.hasNext) recv(Some(ss.next))
          else recv(None)
          go(ss, next, acc)
        case Emit(h, t) => go(ss, t, g(acc, h))
      }
    val s = io.Source.fromFile(f)
    try go(s.getLines, p, z)
    finally s.close
  }
}
