package Part2.Chapter7

import java.util.concurrent.{Callable, CountDownLatch, ExecutorService}

object NonBlockingPar {
  trait Future[+A] {
    private[Chapter7] def apply(cb: A => Unit): Unit
  }

  type Par[+A] = ExecutorService => Future[A]

  def run[A](es: ExecutorService)(p: Par[A]): A = {
    val ref   = new java.util.concurrent.atomic.AtomicReference[A]
    val latch = new CountDownLatch(1)
    p(es) { a => ref.set(a); latch.countDown }
    latch.await
    ref.get
  }

  def unit[A](a: A): Par[A] =
    es =>
      new Future[A] {
        def apply(cb: A => Unit): Unit =
          cb(a)
      }

  /** A non-strict version of `unit` */
  def delay[A](a: => A): Par[A] =
    es =>
      new Future[A] {
        def apply(cb: A => Unit): Unit =
          cb(a)
      }

  def fork[A](a: => Par[A]): Par[A] =
    es =>
      new Future[A] {
        def apply(cb: A => Unit): Unit =
          eval(es)(a(es)(cb))
      }

  def async[A](f: (A => Unit) => Unit): Par[A] = es =>
    new Future[A] {
      def apply(k: A => Unit) = f(k)
    }

  def eval(es: ExecutorService)(r: => Unit): Unit =
    es.submit(new Callable[Unit] { def call = r })

  def map2[A, B, C](p: Par[A], p2: Par[B])(f: (A, B) => C): Par[C] =
    es =>
      new Future[C] {
        def apply(cb: C => Unit): Unit = {
          var ar: Option[A] = None
          var br: Option[B] = None
          val combiner = Actor[Either[A, B]](es) {
            case Left(a) =>
              if (br.isDefined) eval(es)(cb(f(a, br.get)))
              else ar = Some(a)
            case Right(b) =>
              if (ar.isDefined) eval(es)(cb(f(ar.get, b)))
              else br = Some(b)
          }
          p(es)(a => combiner ! Left(a))
          p2(es)(b => combiner ! Right(b))
        }
      }

  def map[A, B](p: Par[A])(f: A => B): Par[B] =
    es =>
      new Future[B] {
        def apply(cb: B => Unit): Unit =
          p(es)(a => eval(es) { cb(f(a)) })
      }

  def lazyUnit[A](a: => A): Par[A] =
    fork(unit(a))

  def asyncF[A, B](f: A => B): A => Par[B] =
    a => lazyUnit(f(a))

  def sequenceRight[A](as: List[Par[A]]): Par[List[A]] =
    as match {
      case Nil    => unit(Nil)
      case h :: t => map2(h, fork(sequence(t)))(_ :: _)
    }

  def sequenceBalanced[A](as: IndexedSeq[Par[A]]): Par[IndexedSeq[A]] = fork {
    if (as.isEmpty) unit(Vector())
    else if (as.length == 1) map(as.head)(a => Vector(a))
    else {
      val (l, r) = as.splitAt(as.length / 2)
      map2(sequenceBalanced(l), sequenceBalanced(r))(_ ++ _)
    }
  }

  def sequence[A](as: List[Par[A]]): Par[List[A]] =
    map(sequenceBalanced(as.toIndexedSeq))(_.toList)

  def parMap[A, B](as: List[A])(f: A => B): Par[List[B]] =
    sequence(as.map(asyncF(f)))

  def parMap[A, B](as: IndexedSeq[A])(f: A => B): Par[IndexedSeq[B]] =
    sequenceBalanced(as.map(asyncF(f)))

  def choice[A](p: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] =
    es => new Future[A] {
      def apply(cb: A => Unit): Unit =
        p(es) { b =>
          if (b) eval(es) { t(es)(cb) }
          else eval(es) { f(es)(cb) }
        }
    }

  def choiceN[A](n: Par[Int])(choices: List[Par[A]]): Par[A] =
    es => new Future[A] {
      def apply(cb: A => Unit): Unit = n(es) {
        ind => eval(es)(choices(ind)(es)(cb))
      }
    }

  /**
   * EXERCISE 7.11
   */
  def choiceViachoiceN[A](cond: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] =
    choiceN(map(cond)(b => if(b) 1 else 0))(List(f, t))

  /**
   * EXERCISE 7.12
   */
  def choiceMap[K,V](key: Par[K])(choices: Map[K,Par[V]]): Par[V] =
    es => new Future[V] {
      def apply(cb: V => Unit): Unit = key(es) {
        k => eval(es)(choices(k)(es)(cb))
      }
    }

  /**
   * EXERCISE 7.13
   */
  def chooser[A,B](pa: Par[A])(choices: A => Par[B]): Par[B] =
    es => new Future[B] {
      def apply(cb: B => Unit): Unit = pa(es) {
        c => eval(es)(choices(c)(es)(cb))
      }
    }

  def choiceViaChooser[A](cond: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] =
    chooser(cond)(b => if(b) t else f)

  def choiceViaChoiceN[A](n: Par[Int])(choices: List[Par[A]]): Par[A] =
    chooser(n)(ind => choices(ind))

  /**
   * EXERCISE 7.14
   */
  def join[A](a: Par[Par[A]]): Par[A] =
    es => new Future[A] {
      def apply(cb: A => Unit): Unit = a(es) {
        pa => eval(es)(pa(es)(cb))
      }
    }

  def flatMap[A,B](a: Par[A])(f: A => Par[B]): Par[B] = join(map(a)(f))

  def joinViaFlatMap[A](a: Par[Par[A]]): Par[A] = flatMap(a)(p => p)

}
