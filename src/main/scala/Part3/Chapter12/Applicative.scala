package Part3.Chapter12

import Part3.Chapter10.Monoid
import Part3.Chapter11.Functor

trait Applicative[F[_]] extends Functor[F] {
  self =>

  /**
    * EXERCISE 12.2
    */
  def apply[A, B](fab: F[A => B])(fa: F[A]): F[B] = map2(fab, fa)((ab, a) => ab(a))
  def unit[A](a: => A): F[A]
  def map[A, B](fa: F[A])(f: A => B): F[B]                    = apply(unit(f))(fa)
  def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] = apply(apply(unit((a: A) => (b: B) => f(a, b)))(fa))(fb)

  def traverse[A, B](as: List[A])(f: A => F[B]): F[List[B]] =
    as.foldRight(unit(List[B]()))((a, fbs) => map2(f(a), fbs)(_ :: _))

  /**
    * EXERCISE 12.1
    */
  def sequence[A](fas: List[F[A]]): F[List[A]]     = traverse(fas)(a => a)
  def replicateM[A](n: Int, fa: F[A]): F[List[A]]  = traverse(List.fill(n)(fa))(a => a)
  def product[A, B](fa: F[A], fb: F[B]): F[(A, B)] = map2(fa, fb)((a, b) => a -> b)

  /**
    * EXERCISE 12.3
    */
  def map3[A, B, C, D](fa: F[A], fb: F[B], fc: F[C])(f: (A, B, C) => D): F[D] =
    apply(apply(apply(unit(f.curried))(fa))(fb))(fc)
  def map4[A, B, C, D, E](fa: F[A], fb: F[B], fc: F[C], fd: F[D])(f: (A, B, C, D) => E): F[E] =
    apply(apply(apply(apply(unit(f.curried))(fa))(fb))(fc))(fd)

  /**
    * EXERCISE 12.8
    */
  def product[G[_]](G: Applicative[G]): Applicative[Lambda[x => (F[x], G[x])]] = {
    new Applicative[Lambda[x => (F[x], G[x])]] {
      override def unit[A](a: => A): (F[A], G[A]) = self.unit(a) -> G.unit(a)

      override def map2[A, B, C](fa: (F[A], G[A]), fb: (F[B], G[B]))(f: (A, B) => C): (F[C], G[C]) =
        self.apply(self.map(fa._1)((a: A) => (b: B) => f(a, b)))(fb._1) ->
          G.apply(G.map(fa._2)((a: A) => (b: B) => f(a, b)))(fb._2)

      override def apply[A, B](fab: (F[A => B], G[A => B]))(fa: (F[A], G[A])): (F[B], G[B]) =
        self.apply(fab._1)(fa._1) ->
          G.apply(fab._2)(fa._2)
    }
  }

  /**
    * EXERCISE 12.9
    */
  def compose[G[_]](G: Applicative[G]): Applicative[Lambda[x => F[G[x]]]] = new Applicative[Lambda[x => F[G[x]]]] {
    override def unit[A](a: => A): F[G[A]] = self.unit(G.unit(a))

    override def map2[A, B, C](fa: F[G[A]], fb: F[G[B]])(f: (A, B) => C): F[G[C]] =
      self.map2(fa, fb)((ga, gb) => G.map2(ga, gb)(f))
  }

  /**
    * EXERCISE 12.12
    */
  def sequenceMap[K, V](ofa: Map[K, F[V]]): F[Map[K, V]] =
    ofa.foldLeft(unit(Map.empty[K, V]))((fm, elem) => map2(fm, elem._2)((m, v) => m.updated(elem._1, v)))
}

object Applicative {
  val streamApplicative = new Applicative[LazyList] {
    def unit[A](a: => A): LazyList[A] =
      LazyList.continually(a)
    override def map2[A, B, C](a: LazyList[A], b: LazyList[B])(f: (A, B) => C): LazyList[C] =
      a zip b map f.tupled
  }

  type Const[M, B] = M
  implicit def monoidApplicative[M](M: Monoid[M]): Applicative[Const[M, *]] =
    new Applicative[Const[M, *]] {
      def unit[A](a: => A): M                                     = M.zero
      override def map2[A, B, C](m1: M, m2: M)(f: (A, B) => C): M = M.op(m1, m2)
    }
}
