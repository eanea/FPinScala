package Part4.Chapter13

import Part2.Chapter7.NonBlockingPar._

trait Translate[F[_], G[_]] { def apply[A](f: F[A]): G[A] }
object Translate {
  type ~>[F[_], G[_]] = Translate[F, G]

  val consoleToFunction0 =
    new (Console ~> Function0) { def apply[A](a: Console[A]) = a.toThunk }
  val consoleToPar: Console ~> Par =
    new (Console ~> Par) { def apply[A](a: Console[A]) = a.toPar }
}
