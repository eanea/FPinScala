package Part1.Chapter6

case class State[S, +A](run: S => (A, S)) {

  /**
   * EXERCISE 6.10
   */
  def flatMap[B](g: A => State[S, B]): State[S, B] = State(s => {
    val (a, sa) = run(s)
    g(a).run(sa)
  })

  def map[B](f: A => B): State[S, B] = this.flatMap(x => State.unit(f(x)))

  def map2[B, C](fb: State[S, B])(f: (A, B) => C): State[S, C] = this.flatMap { a =>
  {
    fb.flatMap { b => State.unit(f(a, b)) }
  }
  }
}

object State {
  def unit[S, A](a: A): State[S, A] = State(s => (a, s))

  def sequence[S, A](fs: List[State[S, A]]): State[S, List[A]] = fs.foldRight(unit[S, List[A]](List.empty[A])) {
    (r, acc) => r.map2(acc)(_ :: _)
  }

  def get[S]: State[S, S]          = State(s => (s, s))
  def set[S](s: S): State[S, Unit] = State(_ => ((), s))
  def modify[S](f: S => S): State[S, Unit] = for {
    s <- get
    _ <- set(f(s))
  } yield ()
}
