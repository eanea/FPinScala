package Part2.Chapter8

import java.util.concurrent.Executors

import Part1.Chapter6.RNG
import Part1.Chapter6.RNG.SimpleRNG
import Part2.Chapter7.Par.Par
import Part2.Chapter8.Prop.{MaxSize, TestCases}
import Part2.Chapter8.Result.{Falsified, Passed, Proved}

case class Prop(run: (MaxSize, TestCases, RNG) => Result) {

  /**
    * EXERCISE 8.9
    */
  def &&(p: Prop): Prop = Prop { (maxSize, n, rng) =>
    run(maxSize, n, rng) match {
      case Passed | Proved               => p.run(maxSize, n, rng)
      case Falsified(failure, successes) => p.label(failure).run(maxSize, n, rng)
    }
  }

  def ||(p: Prop): Prop = Prop { (maxSize, n, rng) =>
    run(maxSize, n, rng) match {
      case Falsified(failure, successes) => p.label(failure).run(maxSize, n, rng)
      case p                             => p
    }
  }

  def label(msg: String): Prop = Prop { (maxSize, n, rng) =>
    run(maxSize, n, rng) match {
      case Falsified(failure, successes) => Falsified(s"$msg \n $failure", successes)
      case p                             => p
    }
  }
}

object Prop {
  type MaxSize      = Int
  type TestCases    = Int
  type FailedCase   = String
  type SuccessCount = Int

  def forAll[A](as: Gen[A])(f: A => Boolean): Prop = Prop { (maxSize, n, rng) =>
    randomStream(as)(rng)
      .zip(LazyList.from(0))
      .take(n)
      .map {
        case (a, i) =>
          try {
            if (f(a)) Passed else Falsified(a.toString, i)
          } catch {
            case e: Exception => Falsified(buildMsg(a, e), i)
          }
      }
      .find(_.isFalsified)
      .getOrElse(Passed)
  }

  def forAll[A](g: Int => Gen[A])(f: A => Boolean): Prop = Prop { (maxSize, n, rng) =>
    val casesPerSize = (n + (maxSize - 1)) / maxSize
    val props: LazyList[Prop] =
      LazyList.from(0).take((n min maxSize) + 1).map(i => forAll(g(i))(f))
    val prop: Prop =
      props.map(p => Prop { (max, _, rng) => p.run(max, casesPerSize, rng) }).toList.reduce(_ && _)
    prop.run(maxSize, n, rng)
  }

  def forAll[A](g: SGen[A])(f: A => Boolean): Prop = forAll(g.forSize)(f)

  def randomStream[A](g: Gen[A])(rng: RNG): LazyList[A] =
    LazyList.unfold(rng)(rng => Some(g.sample.run(rng)))

  def buildMsg[A](s: A, e: Exception): String =
    s"test case: $s\n" +
      s"generated an exception: ${e.getMessage}\n" +
      s"stack trace:\n ${e.getStackTrace.mkString("\n")}"

  def run(p: Prop, maxSize: Int = 100, testCases: Int = 100, rng: RNG = SimpleRNG(System.currentTimeMillis)): Unit =
    p.run(maxSize, testCases, rng) match {
      case Falsified(msg, n) =>
        println(s"! Falsified after $n passed tests:\n $msg")
      case Passed =>
        println(s"+ OK, passed $testCases tests.")
      case Proved =>
        println(s"+ OK, proved property.")
    }

  /**
    * EXERCISE 8.14
    */
  val sortedProp = { gen: Gen[Int] =>
    forAll(SGen.listOf(gen)) {
      case Nil                     => true
      case ::(head, ::(next, Nil)) => head < next
      case l @ ::(head, tail) =>
        !l.sorted.zip(tail).exists { case (a, b) => a > b }
    }
  }

  def check(p: => Boolean): Prop = Prop { (_, _, _) => if (p) Passed else Falsified("()", 0) }

  val S = Gen.weighted(
    Gen.choose(1,4).map(Executors.newFixedThreadPool) -> .75,
    Gen.unit(Executors.newCachedThreadPool) -> .25)

  def forAllPar[A](g: Gen[A])(f: A => Par[Boolean]): Prop =
    forAll(S ** g) { case s ** a => f(a)(s).get }

  def checkPar(p: Par[Boolean]): Prop =
    forAllPar(Gen.unit(()))(_ => p)

  object ** {
    def unapply[A,B](p: (A,B)): Some[(A, B)] = Some(p)
  }
}