package Part2.Chapter8

case class SGen[+A](forSize: Int => Gen[A]) {
  /**
   * EXERCISE 8.11
   */
  def map[B](f: A => B): SGen[B] = SGen {
    n => forSize(n).map(f)
  }

  def flatMap[B](f: A => SGen[B]): SGen[B]    = SGen {
    n => forSize(n).flatMap(a => f(a).forSize(n))
  }

  def map2[B, C](g: SGen[B])(f: (A, B) => C): SGen[C] = SGen {
    n => forSize(n).map2(g.forSize(n))(f)
  }

  def **[B](g: SGen[B]): SGen[(A,B)] =
    (this.map2(g))((_,_))
}

object SGen {
  /**
   * EXERCISE 8.12
   */
  def listOf[A](g: Gen[A]): SGen[List[A]] = SGen {
    n => Gen.listOfN(n, g)
  }

  /**
   * EXERCISE 8.13
   */
  def listOf1[A](g: Gen[A]): SGen[List[A]] = SGen {
    n => g.listOfN(Gen.unit(n max 1))
  }
}