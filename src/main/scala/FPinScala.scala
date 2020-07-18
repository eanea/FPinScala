import scala.collection.immutable.{List => standartList, Nil => standartNil}

sealed trait List[+A]

case object Nil extends List[Nothing]

case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {

  /**
   * EXERCISE 3.10
   */
  def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B = as match {
    case Nil => z
    case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
  }

  /**
   * EXERCISE 3.11
   * @param l
   * @return
   */
  def sum3(l: List[Int]) = foldLeft(l, 0)(_ + _)
  def product3_11(l: List[Double]) = foldLeft(l, 1.0)(_ * _)
  def length[A](l: List[A]) = foldLeft(l, 0)((acc, _) => acc + 1)

  /**
   * EXERCISE 3.12
   */

  def reverse[A](l: List[A]) = foldLeft(l, List[A]())((acc, x) => Cons(x, acc))

  /**
   * EXERCISE 3.7
   */
  def product3(ns: List[Double]): Double = ???


  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B =
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))

    }

  def sum2(ns: List[Int]): Int = foldRight(ns, 0)((x, y) => x + y)

  def product2(ns: List[Double]): Double = foldRight(ns, 1.0)(_ * _)

  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x, xs) => x + sum(xs)
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  /**
   * EXERCISE 3.2
   * @param l
   * @tparam A
   * @return
   */
  def tail[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case Cons(x, xs) => xs
  }

  /**
   * EXERCISE 3.3
   * @param a
   * @param l
   * @tparam A
   * @return
   */
  def setHead[A](a: A, l: List[A]): List[A] = l match {
    case Nil => List(a)
    case Cons(x, xs) => Cons(a, xs)
  }

  /**
   * EXERCISE 3.4
   * @param n
   * @param l
   * @tparam A
   * @return
   */
  @scala.annotation.tailrec
  def drop[A](n: Int, l: List[A]): List[A] = n match {
    case 0 => l
    case _ => drop(n - 1, tail(l))
  }

  /**
   * EXERCISE 3.5
   * @param l
   * @param f
   * @tparam A
   * @return
   */
  @scala.annotation.tailrec
  def dropWhile[A](l: List[A])(f: A => Boolean): List[A] = l match {
    case Nil => l
    case Cons(x, xs) if f(x) => dropWhile(xs)(f)
  }

  def append[A](a1: List[A], a2: List[A]): List[A] = {
    a1 match {
      case Nil => a2
      case Cons(h, t) => Cons(h, append(t, a2))
    }
  }

  /**
   * EXERCISE 3.6
   * @param l
   * @tparam A
   * @return
   */
  def init[A](l: List[A]): List[A] = l match {
    case Cons(x, Nil) => Nil
    case Cons(x, xs) => Cons(x, init(xs))
  }

  /**
   * EXERCISE 3.9
   */
  def length3_9[A](l: List[A]): Int = foldRight(l, 0)((_, acc) => acc + 1)
}

object FPinScala {

  /***
   * EXERCISE 2.1
   * @param n
   * @return
   */
  def fib(n: Int): Int = {
    @scala.annotation.tailrec
    def go(x: Int, y: Int, m: Int): Int = {
      if (m == n) x + y
      else go(y, x + y, m + 1)
    }

    go(0, 1, 2)
  }

  def partial1[A, B, C](a: A, f: (A, B) => C): B => C = {
    b: B => f(a, b)
  }

  /**
   * EXERCISE 2.2
   * @param as
   * @param ordered
   * @tparam A
   * @return
   */
  def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {
    @scala.annotation.tailrec
    def loop(i: Int): Boolean = {
      if (i + 1 >= as.length) true
      else if (ordered(as(i), as(i + 1))) loop(i + 1)
      else false
    }

    loop(0)
  }

  /**
   * EXERCISE 2.3
   * @param f
   * @tparam A
   * @tparam B
   * @tparam C
   * @return
   */
  def curry[A, B, C](f: (A, B) => C): A => (B => C) = {
    a: A => (b: B) => f(a, b)
  }


  /**
   * EXERCISE 2.4
   * @param f
   * @tparam A
   * @tparam B
   * @tparam C
   * @return
   */
  def uncurry[A, B, C](f: A => B => C): (A, B) => C = {
    (a: A, b: B) => f(a)(b)
  }

  /**
   * EXERCISE 2.5
   * @param f
   * @param g
   * @tparam A
   * @tparam B
   * @tparam C
   * @return
   */
  def compose[A, B, C](f: B => C, g: A => B): A => C = {
    a: A => f(g(a))
  }

  def main(args: Array[String]) {

    val list = List(1, 2, 3, 5, 5)

    val l = List.reverse(list)

    println(l)
  }
}