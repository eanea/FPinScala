package Part4.Chapter13

import Part2.Chapter7.NonBlockingPar._
import Part3.Chapter12.Monad
import Part4.Chapter13.Console.{ConsoleReader, ConsoleState}
import Part4.Chapter13.Free.Suspend
import Part4.Chapter13.Translate.~>

import scala.io.StdIn

sealed trait Console[A] {
  def toPar: Par[A]
  def toThunk: () => A
  def toReader: ConsoleReader[A]
  def toState: ConsoleState[A]
}

object Console {

  case object ReadLine extends Console[Option[String]] {

    def toPar = lazyUnit(run)

    def toThunk = () => run

    def run: Option[String] =
      try Some(StdIn.readLine)
      catch {
        case e: Exception => None
      }

    override def toReader: ConsoleReader[Option[String]] = ConsoleReader{ str => Some(str)}

    override def toState: ConsoleState[Option[String]] = ConsoleState{
      s =>
        s.in match {
          case ::(head, next) => (Some(head), Buffers(next, head :: s.out))
          case Nil => (None, s)
        }
    }
  }

  case class PrintLine(line: String) extends Console[Unit] {
    def toPar = lazyUnit(println(line))

    def toThunk = () => println(line)

    override def toReader: ConsoleReader[Unit] = ConsoleReader {
      str => ()
    }

    override def toState: ConsoleState[Unit] = ConsoleState {
      s =>((), s.copy(out = line :: s.out))
    }
  }

  type ConsoleIO[A] = Free[Console, A]

  def readLn: ConsoleIO[Option[String]] =
    Suspend(ReadLine)

  def printLn(line: String): ConsoleIO[Unit] =
    Suspend(PrintLine(line))

  case class ConsoleReader[A](run: String => A) {
    def map[B](f: A => B): ConsoleReader[B] =
      ConsoleReader(r => f(run(r)))

    def flatMap[B](f: A => ConsoleReader[B]): ConsoleReader[B] =
      ConsoleReader(r => f(run(r)).run(r))
  }

  object ConsoleReader {
    implicit val monad = new Monad[ConsoleReader] {
      def unit[A](a: => A) = ConsoleReader(_ => a)

      override def flatMap[A, B](ra: ConsoleReader[A])(f: A => ConsoleReader[B]) =
        ra flatMap f
    }
  }

  val consoleToReader = new (Console ~> ConsoleReader) {
    def apply[A](a: Console[A]) = a.toReader
  }

  def runConsoleReader[A](io: ConsoleIO[A]): ConsoleReader[A] =
    Free.runFree[Console, ConsoleReader, A](io)(consoleToReader)

  case class Buffers(in: List[String], out: List[String])

  case class ConsoleState[A](run: Buffers => (A, Buffers))

  object ConsoleState {
    implicit val monad: Monad[ConsoleState] = new Monad[ConsoleState] {
      override def flatMap[A, B](fa: ConsoleState[A])(f: A => ConsoleState[B]): ConsoleState[B] = ???

      override def unit[A](a: => A): ConsoleState[A] = ???
    }
  }

  val consoleToState = new (Console ~> ConsoleState) {
    def apply[A](a: Console[A]) = a.toState
  }

  def runConsoleState[A](io: ConsoleIO[A]): ConsoleState[A] =
   Free.runFree[Console, ConsoleState, A](io)(consoleToState)
}
