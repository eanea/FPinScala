package Part4.Chapter13

import java.util.concurrent.ExecutorService

import Part2.Chapter7.Par
import Part2.Chapter7.Par.Par
import Part3.Chapter12.Monad
import Part4.Chapter13.IO.{FlatMap, Return, Suspend}

sealed trait IO[A] {
  def flatMap[B](f: A => IO[B]): IO[B] =
    FlatMap(this, f)

  def map[B](f: A => B): IO[B] =
    flatMap(f andThen (Return(_)))
}

object IO {
  def apply[A](a: => A): IO[A] = Return(a)

  case class Return[A](a: A) extends IO[A]

  case class Suspend[A](resume: () => A) extends IO[A]

  case class FlatMap[A, B](sub: IO[A], k: A => IO[B]) extends IO[B]

  def printLine(s: String): IO[Unit] =
    Suspend(() => Return(println(s)))

  @annotation.tailrec
  def run[A](io: IO[A]): A = io match {
    case Return(a) => a

    case Suspend(r) => r()

    case FlatMap(x, f) =>
      x match {

        case Return(a) => run(f(a))

        case Suspend(r) => run(f(r()))

        case FlatMap(y, g) => run(y flatMap (a => g(a) flatMap f))
      }
  }
}
