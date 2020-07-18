package Part3.Chapter12

import Part1.Chapter6.State
import Part2.Chapter7.Par
import Part2.Chapter7.Par.Par

trait Monad[F[_]] extends Applicative[F] {
  def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B]
  def join[A](ffa: F[F[A]]): F[A]                 = flatMap(ffa)(fa => fa)
  def compose[A, B, C](f: A => F[B], g: B => F[C]): A => F[C] =
    a => flatMap(f(a))(g)
  override def map[A, B](fa: F[A])(f: A => B): F[B] =
    flatMap(fa)((a: A) => unit(f(a)))
  override def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] =
    flatMap(fa)(a => map(fb)(b => f(a, b)))
}

object Monad {

  /**
    * EXERCISE 12.5
    */
  def eitherMonad[E]: Monad[Either[E, *]] = new Monad[Either[E, *]] {
    override def unit[A](a: => A): Either[E, A] = Right(a)

    override def flatMap[A, B](fa: Either[E, A])(f: A => Either[E, B]): Either[E, B] = fa match {
      case Left(value)  => Left(value)
      case Right(value) => f(value)
    }
  }

  type Id[A] = A

  val idMonad: Monad[Id] = new Monad[Id] {
    override def unit[A](a: => A): Id[A] = a

    override def flatMap[A, B](ma: Id[A])(f: A => Id[B]): Id[B] = f(ma)
  }

  def stateMonad[S]: Monad[State[S, *]] = new Monad[State[S, *]] {
    def unit[A](a: => A): State[S, A] = State(s => (a, s))
    override def flatMap[A, B](st: State[S, A])(f: A => State[S, B]): State[S, B] =
      st flatMap f
  }

  /**
   * EXERCISE 12.20
   */
  def composeM[G[_], H[_]](implicit G: Monad[G], H: Monad[H], T: Traverse[H]): Monad[Lambda[x => G[H[x]]]] =
    new Monad[Lambda[x => G[H[x]]]] {
      override def unit[A](a: => A): G[H[A]] = G.unit(H.unit(a))

      override def flatMap[A, B](gha: G[H[A]])(f: A => G[H[B]]): G[H[B]] =
        G.flatMap(gha)(ha => G.map(T.traverse(ha)(f))(hhb => H.join(hhb)))
    }


  val parMonad = new Monad[Par] {
    override def unit[A](a: => A): Par[A] = Par.lazyUnit(a)

    override def flatMap[A, B](ma: Par[A])(f: A => Par[B]): Par[B] = Par.flatMap(ma)(f)
  }
}
