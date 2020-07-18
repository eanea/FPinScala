package Part1.Chapter6

trait RNG {
  val seed: Long
  def nextInt: (Int, RNG)
}

object RNG {
  case class SimpleRNG(seed: Long) extends RNG {
    override def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL) & 0xFFFFFFFFFFFFL
      val nextRNG = SimpleRNG(newSeed)
      val n       = (newSeed >>> 16).toInt
      (n, nextRNG)
    }
  }

  type State[S, +A] = S => (A, S)

  /**
    * EXERCISE 6.1
    */
  @scala.annotation.tailrec
  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (n, nextRNG) = rng.nextInt
    if (n == Int.MinValue) nonNegativeInt(nextRNG)
    else if (n < 0) (-n, nextRNG)
    else (n, nextRNG)
  }

  /**
    * EXERCISE 6.2
    */
  def double(rng: RNG): (Double, RNG) = {
    val (n, nextRNG) = nonNegativeInt(rng)
    val d =
      if (n == Int.MaxValue)
        (n - 1).toDouble / Int.MaxValue.toDouble
      else n.toDouble / Int.MaxValue.toDouble
    (d, nextRNG)
  }

  def boolean(rng: RNG): (Boolean, RNG) =
    rng.nextInt match { case (i,iRNG) => (i%2==0,iRNG) }

  /**
    * EXERCISE 6.3
    */
  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val (n, nRNG) = rng.nextInt
    val (d, dRNG) = double(nRNG)
    ((n, d), dRNG)
  }
  def doubleInt(rng: RNG): ((Double, Int), RNG) = {
    val ((n, d), nextRNG) = intDouble(rng)
    ((d, n), nextRNG)
  }
  def double3(rng: RNG): ((Double, Double, Double), RNG) = {
    val (d1, dRNG1) = double(rng)
    val (d2, dRNG2) = double(dRNG1)
    val (d3, dRNG3) = double(dRNG2)
    ((d1, d2, d3), dRNG3)
  }

  /**
    * EXERCISE 6.4
    */
  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    @scala.annotation.tailrec
    def go(c: Int, nextRNG: RNG, ints: List[Int]): (List[Int], RNG) = {
      if (c == 0) (ints, nextRNG)
      else {
        val (n, nRNG) = nextRNG.nextInt
        go(c - 1, nRNG, n :: ints)
      }
    }
    go(count, rng, Nil)
  }

  type Rand[A] = State[RNG, A]
  def unit[A](a: A): Rand[A] = rng => (a, rng)
  val int: Rand[Int]         = _.nextInt
  def map[A, B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  /**
    * EXERCISE 6.5
    */
  def doubleViaMap: Rand[Double] = map(nonNegativeInt)(x => (x - 1).toDouble / Int.MaxValue)

  /**
    * EXERCISE 6.6
    */
  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = { rng =>
    {
      val (a, aRNG) = ra(rng)
      val (b, bRNG) = rb(aRNG)
      (f(a, b), bRNG)
    }
  }

  def both[A, B](ra: Rand[A], rb: Rand[B]): Rand[(A, B)] =
    map2(ra, rb)((_, _))

  val randIntDouble: Rand[(Int, Double)] =
    both(int, double)
  val randDoubleInt: Rand[(Double, Int)] =
    both(double, int)

  /**
    * EXERCISE 6.7
    */
  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = fs.foldRight(unit(Nil: List[A]))((r, acc) => map2(r, acc)(_ :: _))

  /**
    * EXERCISE 6.8
    */
  def flatMap[A, B](f: Rand[A])(g: A => Rand[B]): Rand[B] = { rng =>
    {
      val (n, nextRNG) = f(rng)
      g(n)(nextRNG)
    }
  }
  def nonNegativeLessThan(n: Int): Rand[Int] = flatMap(nonNegativeInt) { i =>
    val mod = i % n
    if (i + (n - 1) - mod >= 0) unit(mod)
    else nonNegativeLessThan(n)
  }

  /**
    * EXERCISE 6.9
    */
  def mapViaFlatMap[A, B](s: Rand[A])(f: A => B): Rand[B] = flatMap(s)(x => unit(f(x)))
  def map2ViaFlatMap[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = flatMap(ra) { a =>
    {
      flatMap(rb) { b => unit(f(a, b)) }
    }
  }

  def rollDie: Rand[Int] = map(nonNegativeLessThan(6))(_ + 1)

//  println(rollDie(SimpleRNG(5))._1)
}
