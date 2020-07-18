package Part1.Chapter4

import Part1.Chapter4.MyEither.{MyLeft, MyRight}

/**
 * EXERCISE 4.6
 */
sealed trait MyEither[+E, +A] {
  def map[B](f: A => B): MyEither[E, B] = this match {
    case MyLeft(v)  => MyLeft(v)
    case MyRight(v) => MyRight(f(v))
  }
  def flatMap[EE >: E, B](f: A => MyEither[EE, B]): MyEither[EE, B] = this match {
    case MyLeft(v)  => MyLeft(v)
    case MyRight(v) => f(v)
  }
  def orElse[EE >: E, B >: A](b: => MyEither[EE, B]): MyEither[EE, B] = this match {
    case MyLeft(_)  => b
    case MyRight(_) => this
  }
  def map2[EE >: E, B, C](b: MyEither[EE, B])(f: (A, B) => C): MyEither[EE, C] = (this, b) match {
    case (MyRight(v1), MyRight(v2)) => MyRight(f(v1, v2))
    case (MyLeft(v1), _)            => MyLeft(v1)
    case (_, MyLeft(v2))            => MyLeft(v2)
  }
}

object MyEither {
  case class MyLeft[+E](value: E)  extends MyEither[E, Nothing]
  case class MyRight[+A](value: A) extends MyEither[Nothing, A]

  def TryE[A](a: => A): MyEither[Exception, A] =
    try MyRight(a)
    catch { case e: Exception => MyLeft(e) }

  /**
    * EXERCISE 4.7
    */
  def sequence[E, A](es: List[MyEither[E, A]]): MyEither[E, List[A]] =
    es.foldRight[MyEither[E, List[A]]](MyRight(Nil))((x, acc) =>
      x match {
        case MyLeft(v)  => MyLeft(v)
        case MyRight(v) => acc.map(l => v :: l)
      }
    )

  def traverse[E, A, B](as: List[A])(f: A => MyEither[E, B]): MyEither[E, List[B]] =
    as.foldRight[MyEither[E, List[B]]](MyRight(Nil))((x, acc) =>
      f(x) match {
        case MyLeft(value)  => MyLeft(value)
        case MyRight(value) => acc.map(l => value :: l)
      }
    )
}
