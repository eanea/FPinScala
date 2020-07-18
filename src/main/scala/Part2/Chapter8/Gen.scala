package Part2.Chapter8

import Part1.Chapter6.{RNG, State}

case class Gen[+A](sample: State[RNG, A]) {

  /**
    * EXERCISE 8.6
    */
  def map[B](f: A => B): Gen[B] = Gen(sample.map(f))
  def flatMap[B](f: A => Gen[B]): Gen[B]    = Gen(sample.flatMap(f(_).sample))
  def listOfN(size: Gen[Int]): Gen[List[A]] = size.flatMap(n => Gen.listOfN(n, this))

  /**
   * EXERCISE 8.10
   */
  def unsized: SGen[A] = SGen {
    n => this
  }

  def map2[B, C](g: Gen[B])(f: (A, B) => C): Gen[C] =
    Gen(sample.map2(g.sample)(f))

  def **[B](g: Gen[B]): Gen[(A,B)] =
    (this map2 g)((_,_))
}

object Gen {

  /**
    * EXERCISE 8.4
    */
  def choose(start: Int, stopExclusive: Int): Gen[Int] = {
    Gen(State[RNG, Int](RNG.nonNegativeInt).map(i => start + i % (stopExclusive - start)))
  }

  /**
    * EXERCISE 8.5
    */
  def unit[A](a: => A): Gen[A]                    = Gen(State.unit(a))
  def boolean: Gen[Boolean]                       = Gen(State(RNG.boolean))
  def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] = Gen(State.sequence(List.fill(n)(g.sample)))

  /**
    * EXERCISE 8.7
    */
  def union[A](g1: Gen[A], g2: Gen[A]): Gen[A] = boolean.flatMap(b => if (b) g1 else g2)

  /**
    * EXERCISE 8.8
    */
  def weighted[A](g1: (Gen[A], Double), g2: (Gen[A], Double)): Gen[A] = {
    val g1Threshold = g1._2.abs / (g1._2.abs + g2._2.abs)

    Gen(State(RNG.double).flatMap(d => if (d < g1Threshold) g1._1.sample else g2._1.sample))
  }

  def stringN(n: Int): Gen[String] =
    listOfN(n, choose(0,127)).map(_.map(_.toChar).mkString)

  val string: SGen[String] = SGen(stringN)

}
