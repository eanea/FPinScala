package Part3.Chapter12

sealed trait Validation[+E, +A]

object Validation {
  case class Failure[E](head: E, tail: Vector[E] = Vector()) extends Validation[E, Nothing]
  case class Success[A](a: A)                                extends Validation[Nothing, A]

  object Failure {
    def apply[E](e: E) = new Failure[E](e)
  }

  /**
   * EXERCISE 12.6
   */
  def validationApplicative[E]: Applicative[Validation[E, *]] = new Applicative[Validation[E, *]] {
    override def unit[A](a: => A): Validation[E, A] = Success(a)

    override def map2[A, B, C](fa: Validation[E, A], fb: Validation[E, B])(f: (A, B) => C): Validation[E, C] = (fa, fb) match {
      case (Success(a), Success(b)) => Success(f(a, b))
      case (Failure(h, t), Success(b)) => Failure(h, t)
      case (Success(a), Failure(h, t)) => Failure(h, t)
      case (Failure(h1, t1), Failure(h2, t2)) => Failure(h1, t1 ++ (h2 +: t2))
    }
  }
}