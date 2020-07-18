package Part2.Chapter7

import java.util.concurrent.{Callable, ExecutorService, Executors, Future}

import scala.concurrent.duration.{MILLISECONDS, TimeUnit}

object Par {
  private case class UnitFuture[A](get: A) extends Future[A] {
    def isDone                                  = true
    def get(timeout: Long, units: TimeUnit): A  = get
    def isCancelled                             = false
    def cancel(evenIfRunning: Boolean): Boolean = false
  }

  private case class TimeoutFuture[A, B, C](fa: Future[A], fb: Future[B], f: (A, B) => C) {
    def get(l: Long, timeUnit: TimeUnit): UnitFuture[C] = {
      val start       = System.currentTimeMillis();
      val a           = fa.get(l, timeUnit)
      val finish      = System.currentTimeMillis();
      val timeElapsed = finish - start;
      val b           = fb.get(l - timeElapsed, timeUnit)
      UnitFuture(f(a, b))
    }
  }

  type Par[A] = ExecutorService => Future[A]

  def unit[A](a: A): Par[A]        = (es: ExecutorService) => UnitFuture(a)
  def lazyUnit[A](a: => A): Par[A] = fork(unit(a))
  def fork[A](a: => Par[A]): Par[A] = es =>
    es.submit(new Callable[A] {
      def call: A = a(es).get
    })

  /**
   * EXERCISE 7.1
   */
  def map2[A, B, C](pa: Par[A], pb: Par[B])(f: (A, B) => C): Par[C] = (es: ExecutorService) => {
    val af = pa(es)
    val bf = pb(es)
    UnitFuture(f(af.get, bf.get))
  }

  /**
   * EXERCISE 7.3
   */
  def map2Timeout[A, B, C](pa: Par[A], pb: Par[B])(f: (A, B) => C)(time: Long): Par[C] = (es: ExecutorService) => {
    val af = pa(es)
    val bf = pb(es)
    TimeoutFuture(af, bf, f).get(time, MILLISECONDS)
  }

  /**
   * EXERCISE 7.4
   */
  def asyncF[A, B](f: A => B): A => Par[B] = a => lazyUnit(f(a))

  def map[A, B](pa: Par[A])(f: A => B): Par[B] =
    map2(pa, unit(()))((a, _) => f(a))

  def sortPar(parList: Par[List[Int]]): Par[List[Int]] = map(parList)(_.sorted)

  def parMap[A, B](ps: List[A])(f: A => B): Par[List[B]] = fork {
    val fbs: List[Par[B]] = ps.map(asyncF(f))
    sequence(fbs)
  }


  def parMap[A, B](ps: IndexedSeq[A])(f: A => B): Par[IndexedSeq[B]] = fork {
    val fbs: IndexedSeq[Par[B]] = ps.map(asyncF(f))
    sequence(fbs)
  }

  /**
   * EXERCISE 7.5
   */
  def sequence[A](ps: List[Par[A]]): Par[List[A]] = ps.foldRight(unit(List.empty[A]))((p, acc) => map2(p, acc)(_ :: _))

  def sequence[A](ps: IndexedSeq[Par[A]]): Par[IndexedSeq[A]] = ps.foldRight(unit(IndexedSeq.empty[A]))((p, acc) => map2(p, acc)((a, b) => b :+ a))

  /**
   * EXERCISE 7.6
   */
  def parFilter[A](as: List[A])(f: A => Boolean): Par[List[A]] = fork {
    val lpa = as.map(asyncF { a =>
      if (f(a)) List(a)
      else Nil
    })
    map(sequence(lpa))(_.flatten)
  }

  def map3[A, B, C, D](pa: Par[A], pb: Par[B], pc: Par[C])(f: (A, B, C) => D): Par[D] = {
    val fab = (a: A, b: B) => (c: C) => f(a, b, c)
    val cd  = map2(pa, pb)(fab)
    map2(cd, pc)((g: C => D, c: C) => g(c))
  }

  def map4[A, B, C, D, E](pa: Par[A], pb: Par[B], pc: Par[C], pd: Par[D])(f: (A, B, C, D) => E): Par[E] = {
    map2(map2(map2(pa, pb)((a, b) => (c: C) => (d: D) => f(a, b, c, d)), pc)((g: C => D => E, c: C) => g(c)), pd)((h: D => E, d: D) => h(d))
  }

  def equal[A](e: ExecutorService)(p: Par[A], p2: Par[A]): Boolean =
    p(e).get == p2(e).get

  def delay[A](fa: => Par[A]): Par[A] =
    es => fa(es)

  def run[A](s: ExecutorService)(a: Par[A]): Future[A] = a(s)

  def choice[A](cond: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] =
    es =>
      if (run(es)(cond).get) t(es)
      else f(es)

  def choiceN[A](n: Par[Int])(choices: List[Par[A]]): Par[A] = es =>
    choices(n(es).get())(es)

  /**
   * EXERCISE 7.11
   */
  def choiceViachoiceN[A](cond: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] =
    choiceN(map(cond)(b => if(b) 1 else 0))(List(f, t))

  /**
   * EXERCISE 7.12
   */
  /**
    * EXERCISE 10.7
    */
  def choiceMap[K,V](key: Par[K])(choices: Map[K,Par[V]]): Par[V] =
    es =>
      choices(key(es).get)(es)

  /**
   * EXERCISE 7.13
   */
  def chooser[A,B](pa: Par[A])(choices: A => Par[B]): Par[B] =
    es =>
      choices(pa(es).get)(es)

  def choiceViaChooser[A](cond: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] =
    chooser(cond)(b => if(b) t else f)

  def choiceViaChoiceN[A](n: Par[Int])(choices: List[Par[A]]): Par[A] =
    chooser(n)(ind => choices(ind))

  /**
   * EXERCISE 7.14
   */
  def join[A](a: Par[Par[A]]): Par[A] =
    es =>
      a(es).get()(es)

  def flatMap[A,B](a: Par[A])(f: A => Par[B]): Par[B] = join(map(a)(f))

  def joinViaFlatMap[A](a: Par[Par[A]]): Par[A] = flatMap(a)(p => p)

  def equal[A](p: Par[A], p2: Par[A]): Par[Boolean] =
    Par.map2(p,p2)(_ == _)
}
