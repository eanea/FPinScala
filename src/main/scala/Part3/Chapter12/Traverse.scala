package Part3.Chapter12

import Part3.Chapter10.{Foldable, Monoid}
import Part3.Chapter11.Functor
import Part3.Chapter12.Applicative.Const
import Part3.Chapter12.Monad.{Id, idMonad}
import Applicative.monoidApplicative
import Part1.Chapter6.State
import Part1.Chapter6.State.{get, set}

trait Traverse[F[_]] extends Functor[F] with Foldable[F] {
  self =>
  def traverse[G[_], A, B](fa: F[A])(f: A => G[B])(implicit G: Applicative[G]): G[F[B]] =
    sequence(map(fa)(f))
  def sequence[G[_], A](fga: F[G[A]])(implicit G: Applicative[G]): G[F[A]] =
    traverse(fga)(ga => ga)

  /**
    * EXERCISE 12.14
    */
  def map[A, B](fa: F[A])(f: A => B): F[B] = traverse[Id, A, B](fa)(a => idMonad.unit(f(a)))(idMonad)

  def foldMap[A, M](as: F[A])(f: A => M)(mb: Monoid[M]): M =
    traverse[Const[M, *], A, Nothing](as)(f)(monoidApplicative(mb))

  def traverseS[S, A, B](fa: F[A])(f: A => State[S, B]): State[S, F[B]] =
    traverse[State[S, *], A, B](fa)(f)(Monad.stateMonad)

  def mapAccum[S, A, B](fa: F[A], s: S)(f: (A, S) => (B, S)): (F[B], S) =
    traverseS(fa)((a: A) =>
      (for {
        s1      <- get[S]
        (b, s2) = f(a, s1)
        _       <- set(s2)
      } yield b)
    ).run(s)

  override def toList[A](fa: F[A]): List[A] =
    mapAccum(fa, List[A]())((a, s) => ((), a :: s))._2.reverse

  def zipWithIndex[A](fa: F[A]): F[(A, Int)] =
    mapAccum(fa, 0)((a, s) => ((a, s), s + 1))._1

  /**
    * EXERCISE 12.16
    */
  def reverse[A](fa: F[A]): F[A] = mapAccum(fa, toList(fa).reverse)((_, s) => (s.head, s.tail))._1

  /**
    * EXERCISE 12.17
    */
  override def foldLeft[A, B](as: F[A])(z: B)(f: (B, A) => B): B = mapAccum(as, z)((a, s) => ((), f(s, a)))._2

  def zip[A, B](fa: F[A], fb: F[B]): F[(A, B)] =
    mapAccum(fa, toList(fb)) {
      case (a, Nil)     => sys.error("zip: Incompatible shapes.")
      case (a, b :: bs) => ((a, b), bs)
    }._1

  def zipL[A, B](fa: F[A], fb: F[B]): F[(A, Option[B])] =
    mapAccum(fa, toList(fb)) {
      case (a, Nil)     => ((a, None), Nil)
      case (a, b :: bs) => ((a, Some(b)), bs)
    }._1

  def zipR[A, B](fa: F[A], fb: F[B]): F[(Option[A], B)] =
    mapAccum(fb, toList(fa)) {
      case (b, Nil)     => ((None, b), Nil)
      case (b, a :: as) => ((Some(a), b), as)
    }._1

  /**
    * EXERCISE 12.18
    */
  def fuse[G[_], H[_], A, B](
      fa: F[A]
  )(f: A => G[B], g: A => H[B])(G: Applicative[G], H: Applicative[H]): (G[F[B]], H[F[B]]) =
    traverse[Lambda[x => (G[x], H[x])], A, B](fa)(a => (f(a), g(a)))(G.product(H))

  /**
    * EXERCISE 12.19
    */
  def compose[G[_]](implicit G: Traverse[G]): Traverse[Lambda[x => F[G[x]]]] = new Traverse[Lambda[x => F[G[x]]]] {
    override def traverse[M[_], A, B](fa: F[G[A]])(f: A => M[B])(implicit M: Applicative[M]) =
      self.traverse(fa)(ga => G.traverse(ga)(f))
  }
}

object Traverse {
  case class Tree[+A](head: A, tail: List[Tree[A]])

  /**
    * EXERCISE 12.13
    */
  object listTraverse extends Traverse[List] {
    override def traverse[G[_]: Applicative, A, B](fa: List[A])(f: A => G[B]): G[List[B]] = sequence(fa.map(f))
  }

  object optionTraverse extends Traverse[Option] {
    override def traverse[G[_]: Applicative, A, B](fa: Option[A])(f: A => G[B]): G[Option[B]] = sequence(fa.map(f))
  }

  object treeTraverse extends Traverse[Tree] {
    override def traverse[G[_]: Applicative, A, B](fa: Tree[A])(f: A => G[B]): G[Tree[B]] = {

      def go(lt: List[Tree[A]])(f: A => G[B]): List[Tree[G[B]]] = lt match {
        case ::(head, next) => Tree(f(head.head), go(head.tail)(f)) :: go(next)(f)
        case Nil            => Nil
      }
      val h = f(fa.head)
      val t = go(fa.tail)(f)
      sequence(Tree(h, t))
    }
  }
}
