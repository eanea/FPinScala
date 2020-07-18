package Part1.Chapter3

sealed trait MyList[+A]
object MyList {
  case object MyNil                               extends MyList[Nothing]
  case class MyCons[+A](head: A, tail: MyList[A]) extends MyList[A]

  def apply[A](as: A*): MyList[A] =
    if (as.isEmpty) MyNil
    else MyCons(as.head, apply(as.tail: _*))


  /**
   * EXERCISE 3.2
   *
   * @param l
   * @tparam A
   * @return
   */
  def tail[A](l: MyList[A]): MyList[A] = l match {
    case MyNil         => MyNil
    case MyCons(_, xs) => xs
  }

  /**
   * EXERCISE 3.3
   *
   * @param a
   * @param l
   * @tparam A
   * @return
   */
  def setHead[A](a: A, l: MyList[A]): MyList[A] = l match {
    case MyNil         => MyList(a)
    case MyCons(x, xs) => MyCons(a, xs)
  }

  /**
   * EXERCISE 3.4
   *
   * @param n
   * @param l
   * @tparam A
   * @return
   */
  @scala.annotation.tailrec
  def drop[A](n: Int, l: MyList[A]): MyList[A] = n match {
    case 0 => l
    case _ => drop(n - 1, tail(l))
  }

  /**
   * EXERCISE 3.5
   *
   * @param l
   * @param f
   * @tparam A
   * @return
   */
  @scala.annotation.tailrec
  def dropWhile[A](l: MyList[A])(f: A => Boolean): MyList[A] = l match {
    case MyNil                 => l
    case MyCons(x, xs) if f(x) => dropWhile(xs)(f)
  }

  def append[A](a1: MyList[A], a2: MyList[A]): MyList[A] = {
    a1 match {
      case MyNil        => a2
      case MyCons(h, t) => MyCons(h, append(t, a2))
    }
  }

  /**
   * EXERCISE 3.6
   *
   * @param l
   * @tparam A
   * @return
   */
  def init[A](l: MyList[A]): MyList[A] = l match {
    case MyCons(x, MyNil) => MyNil
    case MyCons(x, xs)    => MyCons(x, init(xs))
  }

  /**
   * EXERCISE 3.7
   */
  def product3(ns: MyList[Double]): Double = ???

  /**
   * EXERCISE 3.9
   */
  def length3_9[A](l: MyList[A]): Int = foldRight(l, 0)((_, acc) => acc + 1)

  /**
   * EXERCISE 3.10
   */
  def foldLeft[A, B](as: MyList[A], z: B)(f: (B, A) => B): B = as match {
    case MyNil => z
    case MyCons(x, xs) => {
      //      println(s"(z, x) = ($z, $x)")
      foldLeft(xs, f(z, x))(f)
    }
  }

  /**
   * EXERCISE 3.11
   *
   * @param l
   * @return
   */
  def sum3(l: MyList[Int]) = foldLeft(l, 0)(_ + _)

  def product3_11(l: MyList[Double]) = foldLeft(l, 1.0)(_ * _)

  def length[A](l: MyList[A]) = foldLeft(l, 0)((acc, _) => acc + 1)

  /**
   * EXERCISE 3.12
   */
  def reverse[A](l: MyList[A]) =
    foldLeft(l, MyList[A]())((acc, x) => MyCons(x, acc))

  def foldRight[A, B](as: MyList[A], z: B)(f: (A, B) => B): B =
    as match {
      case MyNil         => z
      case MyCons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  /**
   * EXERCISE 3.13
   */
  def foldRightWithFoldLeft[A, B](as: MyList[A], z: B)(f: (A, B) => B): B =
    foldLeft(reverse(as), z)((x, y) => f(y, x))

  def foldLeftWithFoldRight[A, B](as: MyList[A], z: B)(f: (B, A) => B): B =
    foldRight(as, z)((x, y) => f(y, x))

  /**
   * EXERCISE 3.14
   */
  def appendWithFoldRight[A](a1: MyList[A], a2: MyList[A]): MyList[A] =
    foldRight(a1, a2)(MyCons(_, _))

  def appendWithFoldLeft[A](a1: MyList[A], a2: MyList[A]): MyList[A] =
    foldLeft(reverse(a1), a2)((x, y) => MyCons(y, x))

  /**
   * EXERCISE 3.15
   */
  def concat[A](l: MyList[MyList[A]]) =
    foldRight(l, MyNil: MyList[A])(append)

  /**
   * EXERCISE 3.16
   */
  def add1(l: MyList[Int]): MyList[Int] =
    foldRight(l, MyNil: MyList[Int])((x, acc) => MyCons(x + 1, acc))

  /**
   * EXERCISE 3.17
   */
  def listToString(l: MyList[Double]): MyList[String] =
    foldRight(l, MyNil: MyList[String])((x, acc) => MyCons(x.toString, acc))

  /**
   * EXERCISE 3.18
   */
  def map[A, B](as: MyList[A])(f: A => B): MyList[B] =
    foldRightWithFoldLeft(as, MyNil: MyList[B])((x, acc) => MyCons(f(x), acc))

  def sum2(ns: MyList[Int]): Int = foldRight(ns, 0)((x, y) => x + y)

  def product2(ns: MyList[Double]): Double = foldRight(ns, 1.0)(_ * _)

  def sum(ints: MyList[Int]): Int = ints match {
    case MyNil         => 0
    case MyCons(x, xs) => x + sum(xs)
  }

  def product(ds: MyList[Double]): Double = ds match {
    case MyNil          => 1.0
    case MyCons(0.0, _) => 0.0
    case MyCons(x, xs)  => x * product(xs)
  }

  /**
   * EXERCISE 3.19
   */
  def filter[A](as: MyList[A])(f: A => Boolean): MyList[A] =
    foldRight(as, MyNil: MyList[A])((x, acc) => {
      if (f(x)) MyCons(x, acc)
      else acc
    })

  /**
   * EXERCISE 3.20
   */
  def flatMap[A, B](as: MyList[A])(f: A => MyList[B]): MyList[B] =
    concat(map(as)(f))

  /**
   * EXERCISE 3.21
   */
  def filterWithFlatMap[A](as: MyList[A])(f: A => Boolean): MyList[A] =
    flatMap(as)(x => {
      if (f(x)) MyList(x)
      else MyNil
    })

  /**
   * EXERCISE 3.22
   */
  def addTwoLists(l1: MyList[Int], l2: MyList[Int]): MyList[Int] =
    (l1, l2) match {
      case (_, MyNil)                     => MyNil
      case (MyNil, _)                     => MyNil
      case (MyCons(x, xs), MyCons(y, ys)) => MyCons(x + y, addTwoLists(xs, ys))
    }

  /**
   * EXERCISE 3.23
   */
  def zipWith[A, B, C](l1: MyList[A], l2: MyList[B])(
    f: (A, B) => C
  ): MyList[C] = (l1, l2) match {
    case (MyCons(x, xs), MyCons(y, ys)) => MyCons(f(x, y), zipWith(xs, ys)(f))
    case _                              => MyNil
  }

  /**
   * EXERCISE 3.24
   */
  def hasSubsequence[A](seq: List[A], sub: List[A]): Boolean = {
    @scala.annotation.tailrec
    def help(s1: List[A], s2: List[A]): Boolean = (s1, s2) match {
      case (_, Nil)                  => true
      case (x :: xs, y :: ys) if x == y => help(xs, ys)
      case _                            => false
    }

    @scala.annotation.tailrec
    def loop(s1: List[A], s2: List[A]): Boolean = s1 match {
      case Nil           => false
      case s if help(s, s2) => true
      case _ :: xs          => loop(xs, s2)
    }

    loop(seq, sub)
  }
}
