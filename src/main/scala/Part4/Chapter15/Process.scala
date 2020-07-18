package Part4.Chapter15

import java.io.FileWriter

import Part4.Chapter13.Free.Suspend
import Part4.Chapter13.Free._
import Part2.Chapter7.NonBlockingPar._

sealed trait Process[F[_], O] {
  import Process._

  def onHalt(f: Throwable => Process[F, O]): Process[F, O] = this match {
    case Halt(e)          => Try(f(e))
    case Emit(h, t)       => Emit(h, t.onHalt(f))
    case Await(req, recv) => Await(req, recv andThen (_.onHalt(f)))
  }

  def ++(p: => Process[F, O]): Process[F, O] =
    this.onHalt {
      case End => p
      case err => Halt(err)
    }

  def flatMap[O2](f: O => Process[F, O2]): Process[F, O2] =
    this match {
      case Halt(err)  => Halt(err)
      case Emit(o, t) => Try(f(o)) ++ t.flatMap(f)
      case Await(req, recv) =>
        Await(req, recv andThen (_ flatMap f))
    }

  def map[O2](f: O => O2): Process[F, O2] = this match {
    case Await(req, recv) => Await(req, recv andThen(_ map f))
    case Emit(head, tail) => Emit(f(head), tail.map(f))
    case Halt(err)        => Halt(err)
  }

  def onComplete(p: => Process[F, O]): Process[F, O] =
    this.onHalt {
      case End => p.asFinalizer
      case err => p.asFinalizer ++ Halt(err)
    }

  def asFinalizer: Process[F, O] = this match {
    case Emit(h, t) => Emit(h, t.asFinalizer)
    case Halt(e)    => Halt(e)
    case Await(req, recv) =>
      await(req) {
        case Left(Kill) => this.asFinalizer
        case x          => recv(x)
      }
  }

  final def drain[O2]: Process[F, O2] = this match {
    case Halt(e)          => Halt(e)
    case Emit(_, t)       => t.drain
    case Await(req, recv) => Await(req, recv andThen (_.drain))
  }

  def repeat: Process[F, O] =
    this ++ this.repeat

  def |>[O2](p2: Process1[O, O2]): Process[F, O2] = {
    p2 match {
      case Halt(e)    => this.kill onHalt { e2 => Halt(e) ++ Halt(e2) }
      case Emit(h, t) => Emit(h, this |> t)
      case Await(req, recv) =>
        this match {
          case Halt(err)          => Halt(err) |> recv(Left(err))
          case Emit(h, t)         => t |> Try(recv(Right(h)))
          case Await(req0, recv0) => await(req0)(recv0 andThen (_ |> p2))
        }
    }
  }
  def pipe[O2](p2: Process1[O, O2]): Process[F, O2] =
    this |> p2

  @annotation.tailrec
  final def kill[O2]: Process[F, O2] = this match {
    case Await(req, recv) =>
      recv(Left(Kill)).drain.onHalt {
        case Kill => Halt(End)
        case e    => Halt(e)
      }
    case Halt(e)    => Halt(e)
    case Emit(h, t) => t.kill
  }

  def filter(f: O => Boolean): Process[F, O] =
    this |> Process.filter(f)

  def tee[O2, O3](p2: Process[F, O2])(t: Tee[O, O2, O3]): Process[F, O3] = {
    t match {
      case Halt(e)    => this.kill onComplete p2.kill onComplete Halt(e)
      case Emit(h, t) => Emit(h, (this tee p2)(t))
      case Await(side, recv) =>
        side.get match {
          case Left(isO) =>
            this match {
              case Halt(e)     => p2.kill onComplete Halt(e)
              case Emit(o, ot) => (ot tee p2)(Try(recv(Right(o))))
              case Await(reqL, recvL) =>
                await(reqL)(recvL andThen (this2 => this2.tee(p2)(t)))
            }
          case Right(isO2) =>
            p2 match {
              case Halt(e)      => this.kill onComplete Halt(e)
              case Emit(o2, ot) => (this tee ot)(Try(recv(Right(o2))))
              case Await(reqR, recvR) =>
                await(reqR)(recvR andThen (p3 => this.tee(p3)(t)))
            }
        }
    }
  }

  def zipWith[O2, O3](p2: Process[F, O2])(f: (O, O2) => O3): Process[F, O3] =
    (this tee p2)(Process.zipWith(f))

  def zip[O2](p2: Process[F, O2]): Process[F, (O, O2)] =
    zipWith(p2)((_, _))

  def to[O2](sink: Sink[F, O]): Process[F, Unit] =
    join { (this zipWith sink)((o, f) => f(o)) }

  def through[O2](p2: Process[F, O => Process[F, O2]]): Process[F, O2] =
    join { (this zipWith p2)((o, f) => f(o)) }

  def take(n: Int): Process[F, O] =
    this |> Process.take(n)

  def once: Process[F, O] = take(1)
}

object Process {
  case class Await[F[_], A, O](req: F[A], recv: Either[Throwable, A] => Process[F, O]) extends Process[F, O]
  case class Emit[F[_], O](head: O, tail: Process[F, O])                               extends Process[F, O]
  def emit[F[_], O](head: O, tail: Process[F, O] = Halt[F, O](End)): Process[F, O] =
    Emit(head, tail)

  def await[F[_], A, O](req: F[A])(recv: Either[Throwable, A] => Process[F, O]): Process[F, O] =
    Await(req, recv)

  case class Halt[F[_], O](err: Throwable) extends Process[F, O]
  case object End                          extends Exception
  case object Kill                         extends Exception

  def Try[F[_], O](p: => Process[F, O]): Process[F, O] =
    try p
    catch { case e: Throwable => Halt(e) }

  def runLog[O](src: Process[IO, O]): IO[IndexedSeq[O]] = IO {
    val E = java.util.concurrent.Executors.newFixedThreadPool(4)
    @annotation.tailrec
    def go(cur: Process[IO, O], acc: IndexedSeq[O]): IndexedSeq[O] =
      cur match {
        case Emit(h, t) => go(t, acc :+ h)
        case Halt(End)  => acc
        case Halt(err)  => throw err
        case Await(req, recv) =>
          val next =
            try recv(Right(unsafePerformIO(req)(E)))
            catch { case err: Throwable => recv(Left(err)) }
          go(next, acc)
      }
    try go(src, IndexedSeq())
    finally E.shutdown
  }

  def resource[R, O](acquire: IO[R])(use: R => Process[IO, O])(release: R => Process[IO, O]): Process[IO, O] =
    eval(acquire) flatMap (r => use(r).onComplete(release(r)))

  /**
    * EXERCISE 15.11
    */
  def eval[F[_], A](a: F[A]): Process[F, A] = await(a) {
    case Left(value)  => Halt(value)
    case Right(value) => Emit(value, Halt(End))
  }

  def eval_[F[_], A, B](a: F[A]): Process[F, B] = eval(a).drain[B]

  def lines(filename: String): Process[IO, String] =
    resource { IO(io.Source.fromFile(filename)) } { src =>
      lazy val iter = src.getLines // a stateful iterator
      def step      = if (iter.hasNext) Some(iter.next) else None
      lazy val lines: Process[IO, String] = eval(IO(step)).flatMap {
        case None       => Halt(End)
        case Some(line) => Emit(line, lines)
      }
      lines
    } { src => eval_ { IO(src.close) } }

  def take[I](n: Int): Process1[I, I] =
    if (n <= 0) halt1
    else await1[I, I](i => emit(i, take(n - 1)))

  case class Is[I]() {
    sealed trait f[X]
    val Get = new f[I] {}
  }

  type Process1[I, O] = Process[Is[I]#f, O]
  def Get[I] = Is[I]().Get

  def await1[I, O](recv: I => Process1[I, O], fallback: Process1[I, O] = halt1[I, O]): Process1[I, O] =
    Await(
      Get[I],
      (e: Either[Throwable, I]) =>
        e match {
          case Left(End) => fallback
          case Left(err) => Halt(err)
          case Right(i)  => Try(recv(i))
        }
    )
  def emit1[I, O](h: O, tl: Process1[I, O] = halt1[I, O]): Process1[I, O] =
    emit(h, tl)
  def halt1[I, O]: Process1[I, O] = Halt[Is[I]#f, O](End)

  def lift[I, O](f: I => O): Process1[I, O] =
    await1[I, O](i => emit(f(i))).repeat
  def filter[I](f: I => Boolean): Process1[I, I] =
    await1[I, I](i => if (f(i)) emit(i) else halt1) repeat

  case class T[I, I2]() {
    sealed trait f[X] { def get: Either[I => X, I2 => X] }
    val L: f[I]  = new f[I]  { def get = Left(identity)  }
    val R: f[I2] = new f[I2] { def get = Right(identity) }
  }
  def L[I, I2]: T[I, I2]#f[I]  = T[I, I2]().L
  def R[I, I2]: T[I, I2]#f[I2] = T[I, I2]().R

  type Tee[I, I2, O] = Process[T[I, I2]#f, O]

  def haltT[I, I2, O]: Tee[I, I2, O] =
    Halt[T[I, I2]#f, O](End)
  def awaitL[I, I2, O](recv: I => Tee[I, I2, O], fallback: => Tee[I, I2, O] = haltT[I, I2, O]): Tee[I, I2, O] =
    await[T[I, I2]#f, I, O](L) {
      case Left(End) => fallback
      case Left(err) => Halt(err)
      case Right(a)  => Try(recv(a))
    }
  def awaitR[I, I2, O](recv: I2 => Tee[I, I2, O], fallback: => Tee[I, I2, O] = haltT[I, I2, O]): Tee[I, I2, O] =
    await[T[I, I2]#f, I2, O](R) {
      case Left(End) => fallback
      case Left(err) => Halt(err)
      case Right(a)  => Try(recv(a))
    }
  def emitT[I, I2, O](h: O, tl: Tee[I, I2, O] = haltT[I, I2, O]): Tee[I, I2, O] =
    emit(h, tl)

  def zipWith[I, I2, O](f: (I, I2) => O): Tee[I, I2, O] =
    awaitL[I, I2, O](i => awaitR(i2 => emitT(f(i, i2)))) repeat
  def zip[I, I2]: Tee[I, I2, (I, I2)] = zipWith((_, _))

  type Sink[F[_], O] = Process[F, O => Process[F, Unit]]

  def fileW(file: String, append: Boolean = false): Sink[IO, String] =
    resource[FileWriter, String => Process[IO, Unit]] { IO { new FileWriter(file, append) } } { w =>
      constant { (s: String) => eval[IO, Unit](IO(w.write(s))) }
    } { w => eval_(IO(w.close())) }
  def constant[A](a: A): Process[IO, A] =
    eval[IO, A](IO(a)).repeat

  /**
    * EXERCISE 15.12
    */
  def join[F[_], O](p: Process[F, Process[F, O]]): Process[F, O] = p.flatMap(x => x)

  type Channel[F[_], I, O] = Process[F, I => Process[F, O]]

  def fahrenheitToCelsius(f: Double): Double =
    (f - 32) * 5.0 / 9.0

  val convertAll: Process[IO, Unit] = (for {
    out  <- fileW("celsius.txt").once
    file <- lines("fahrenheits.txt")
    _    <- lines(file).map(line => fahrenheitToCelsius(line.toDouble)).flatMap(celsius => out(celsius.toString))
  } yield ()) drain
}
