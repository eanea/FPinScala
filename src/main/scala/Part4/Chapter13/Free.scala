package Part4.Chapter13

import java.util.concurrent.ExecutorService

import Part2.Chapter7.NonBlockingPar
import Part3.Chapter12.Monad
import Part4.Chapter13.Free.{FlatMap, Return}
import Part4.Chapter13.Translate.~>
import Translate._

import scala.annotation.tailrec
import Part2.Chapter7.NonBlockingPar._
import sun.security.provider.NativePRNG.NonBlocking

sealed trait Free[F[_], A] {
  self =>
  def flatMap[B](f: A => Free[F, B]): Free[F, B] =
    FlatMap(self, f)
  def map[B](f: A => B): Free[F, B] =
    flatMap(f andThen (Return(_)))
}

object Free {
  type TailRec[A] = Free[Function0, A]
  type Async[A]   = Free[Par, A]

  case class Return[F[_], A](a: A) extends Free[F, A]

  case class Suspend[F[_], A](s: F[A]) extends Free[F, A]

  case class FlatMap[F[_], A, B](s: Free[F, A], f: A => Free[F, B]) extends Free[F, B]

  /**
    * EXERCISE 13.1
    */
  def freeMonad[F[_]]: Monad[Free[F, *]] = new Monad[Free[F, *]] {
    override def unit[A](a: => A): Free[F, A] = Return(a)

    override def flatMap[A, B](fa: Free[F, A])(f: A => Free[F, B]): Free[F, B] = fa.flatMap(f)

  }

  /**
    * EXERCISE 13.2
    */
  @tailrec
  def runTrampoline[A](a: Free[Function0, A]): A = a match {
    case Return(a)  => a
    case Suspend(s) => s()
    case FlatMap(s, f) =>
      s match {
        case Return(a2)    => runTrampoline(f(a2))
        case Suspend(r)    => runTrampoline(f(r()))
        case FlatMap(r, g) => runTrampoline(r flatMap (aa => g(aa) flatMap f))
      }
  }

  /**
    * EXERCISE 13.3
    */
  def run[F[_], A](a: Free[F, A])(implicit F: Monad[F]): F[A] = step(a) match {
    case Return(a)  => F.unit(a)
    case Suspend(r) => r
    case FlatMap(x, f) =>
      x match {
        case Suspend(r) => F.flatMap(r)(a => run(f(a)))
        case _          => sys.error("Impossible; `step` eliminates these cases")
      }
  }

  @annotation.tailrec
  def step[F[_], A](fa: Free[F, A]): Free[F, A] = fa match {
    case FlatMap(FlatMap(x, f), g) => step(x flatMap (a => f(a) flatMap g))
    case FlatMap(Return(x), f)     => step(f(x))
    case _                         => fa
  }

  def runFree[F[_], G[_], A](free: Free[F, A])(t: F ~> G)(implicit G: Monad[G]): G[A] =
    step(free) match {
      case Return(a)              => G.unit(a)
      case Suspend(r)             => t(r)
      case FlatMap(Suspend(r), f) => G.flatMap(t(r))(a => runFree(f(a))(t))
      case _                      => sys.error("Impossible; `step` eliminates these cases")
    }

  implicit val function0Monad = new Monad[Function0] {
    def unit[A](a: => A) = () => a
    override def flatMap[A, B](a: Function0[A])(f: A => Function0[B]) =
      () => f(a())()
  }
  implicit val parMonad = new Monad[Par] {
    def unit[A](a: => A) = NonBlockingPar.unit(a)
    override def flatMap[A, B](a: Par[A])(f: A => Par[B]) =
      fork { NonBlockingPar.flatMap(a)(f) }
  }

  def runConsoleFunction0[A](a: Free[Console, A]): () => A =
    runFree[Console, Function0, A](a)(consoleToFunction0)
  def runConsolePar[A](a: Free[Console, A]): Par[A] =
    runFree[Console, Par, A](a)(consoleToPar)

  def translate[F[_], G[_], A](f: Free[F, A])(fg: F ~> G): Free[G, A] = {
    type FreeG[A] = Free[G, A]
    val FtoG = new ~>[F, FreeG] {
      override def apply[A](f: F[A]): FreeG[A] = Suspend(fg(f))
    }
    runFree(f)(FtoG)(freeMonad[G])
  }
  def runConsole[A](a: Free[Console, A]): A = runTrampoline(translate(a)(consoleToFunction0))

  type IO[A] = Free[Par, A]

  def IO[A](a: => A): IO[A] = Suspend { delay(a) }

  def unsafePerformIO[A](io: IO[A])(implicit E: ExecutorService): A =
    NonBlockingPar.run(E) { run(io)(parMonad) }
}
