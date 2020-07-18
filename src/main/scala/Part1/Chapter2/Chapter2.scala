package Part1.Chapter2

object Chapter2 {
  /** *
   * EXERCISE 2.1
   *
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

  def partial1[A, B, C](a: A, f: (A, B) => C): B => C = { b: B =>
    f(a, b)
  }

  /**
   * EXERCISE 2.2
   *
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
   *
   * @param f
   * @tparam A
   * @tparam B
   * @tparam C
   * @return
   */
  def curry[A, B, C](f: (A, B) => C): A => (B => C) = { a: A => (b: B) =>
    f(a, b)
  }

  /**
   * EXERCISE 2.4
   *
   * @param f
   * @tparam A
   * @tparam B
   * @tparam C
   * @return
   */
  def uncurry[A, B, C](f: A => B => C): (A, B) => C = { (a: A, b: B) =>
    f(a)(b)
  }

  /**
   * EXERCISE 2.5
   *
   * @param f
   * @param g
   * @tparam A
   * @tparam B
   * @tparam C
   * @return
   */
  def compose[A, B, C](f: B => C, g: A => B): A => C = { a: A =>
    f(g(a))
  }
}
