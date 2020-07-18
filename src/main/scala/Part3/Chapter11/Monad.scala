package Part3.Chapter11

import Part1.Chapter6.State
import Part2.Chapter7.Par
import Part2.Chapter7.Par.Par
import Part2.Chapter8.Gen

trait Monad[F[_]] extends Functor[F] {
  def unit[A](a: => A): F[A]
  def flatMap[A, B](ma: F[A])(f: A => F[B]): F[B]
  def map[A, B](ma: F[A])(f: A => B): F[B] =
    flatMap(ma)(a => unit(f(a)))
  def map2[A, B, C](ma: F[A], mb: F[B])(f: (A, B) => C): F[C] =
    flatMap(ma)(a => map(mb)(b => f(a, b)))

  /**
    * EXERCISE 11.3
    */
  def sequence[A](lma: List[F[A]]): F[List[A]]              = lma.foldLeft(unit(List.empty[A]))((acc, fa) => map2(fa, acc)(_ :: _))
  def traverse[A, B](la: List[A])(f: A => F[B]): F[List[B]] = sequence(la.map(f))

  /**
    * EXERCISE 11.4
    */
  def replicateM[A](n: Int, ma: F[A]): F[List[A]]  = sequence(List.fill(n)(ma))
  def product[A, B](ma: F[A], mb: F[B]): F[(A, B)] = map2(ma, mb)((_, _))

  /**
    * EXERCISE 11.6
    */
  def filterM[A](ms: List[A])(f: A => F[Boolean]): F[List[A]] =
    ms.foldLeft(unit(List.empty[A]))((acc, a) => map2(map(f(a))(b => if (b) List(a) else Nil), acc)(_ ++ _))

  /**
    * EXERCISE 11.7
    */
  def compose[A, B, C](f: A => F[B], g: B => F[C]): A => F[C] = a => flatMap(f(a))(g)

  /**
    * EXERCISE 11.8
    */
  def flatMapViaCompose[A, B](ma: F[A])(f: A => F[B]): F[B] = compose((_: Unit) => ma, f)(())

  /**
    * EXERCISE 11.12
    */
  def join[A](mma: F[F[A]]): F[A] = flatMap(mma)(fa => fa)

  /**
    * EXERCISE 11.13
    */
  def composeViaJoin[A, B, C](f: A => F[B], g: B => F[C]): A => F[C] = a => join(map(f(a))(g))
  def flatMapViaJoin[A, B](ma: F[A])(f: A => F[B]): F[B]             = join(map(ma)(f))

}

object Monad {
  val genMonad = new Monad[Gen] {
    def unit[A](a: => A): Gen[A] = Gen.unit(a)
    def flatMap[A, B](ma: Gen[A])(f: A => Gen[B]): Gen[B] =
      ma flatMap f
  }

  /**
    * EXERCISE 11.1
    */
  val parMonad = new Monad[Par] {
    override def unit[A](a: => A): Par[A] = Par.lazyUnit(a)

    override def flatMap[A, B](ma: Par[A])(f: A => Par[B]): Par[B] = Par.flatMap(ma)(f)
  }

  val optionMonad = new Monad[Option] {
    override def unit[A](a: => A): Option[A] = Some(a)

    override def flatMap[A, B](ma: Option[A])(f: A => Option[B]): Option[B] = ma.flatMap(f)
  }

  val lazyListMonad: Monad[LazyList] = new Monad[LazyList] {
    override def unit[A](a: => A): LazyList[A] = LazyList.apply(a)

    override def flatMap[A, B](ma: LazyList[A])(f: A => LazyList[B]): LazyList[B] = ma.flatMap(f)
  }

  val listMonad: Monad[List] = new Monad[List] {
    override def unit[A](a: => A): List[A] = List(a)

    override def flatMap[A, B](ma: List[A])(f: A => List[B]): List[B] = ma.flatMap(f)
  }

  /**
    * EXERCISE 11.17
    */
  case class Id[A](value: A) {
    def map[B](f: A => B): Id[B]         = Id(f(value))
    def flatMap[B](f: A => Id[B]): Id[B] = f(value)
  }

  val idMonad: Monad[Id] = new Monad[Id] {
    override def unit[A](a: => A): Id[A] = Id(a)

    override def flatMap[A, B](ma: Id[A])(f: A => Id[B]): Id[B] = ma.flatMap(f)
  }

  type IntState[A] = State[Int, A]

  object IntStateMonad extends Monad[IntState] {
    def unit[A](a: => A): IntState[A] = State(s => (a, s))
    def flatMap[A, B](st: IntState[A])(f: A => IntState[B]): IntState[B] =
      st flatMap f
  }

  def stateMonad[S]: Monad[State[S, *]] = new Monad[State[S, *]] {
    def unit[A](a: => A): State[S, A] = State(s => (a, s))
    def flatMap[A, B](st: State[S, A])(f: A => State[S, B]): State[S, B] =
      st flatMap f
  }

  case class Reader[R, A](run: R => A)
  object Reader {
    def readerMonad[R]: Monad[Reader[R, *]] = new Monad[Reader[R, *]] {
      def unit[A](a: => A): Reader[R, A]                                      = Reader { _ => a }
      def flatMap[A, B](st: Reader[R, A])(f: A => Reader[R, B]): Reader[R, B] = Reader { r => f(st.run(r)).run(r) }
    }
  }
}
