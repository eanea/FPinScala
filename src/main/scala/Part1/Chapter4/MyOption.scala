package Part1.Chapter4

import Part1.Chapter4.MyOption.{MyNone, MySome}

/**
 * EXERCISE 4.1
 */
sealed trait MyOption[+A] {
  def map[B](f: A => B): MyOption[B] = this match {
    case MySome(v) => MySome(f(v))
    case MyNone    => MyNone
  }
  def getOrElse[B >: A](default: => B): B = this match {
    case MySome(v) => v
    case MyNone    => default
  }
  def flatMap[B](f: A => MyOption[B]): MyOption[B]
  //    = map(f).getOrElse(None)
  def orElse[B >: A](ob: => MyOption[B]): MyOption[B]
  //    = ???
  def filter(f: A => Boolean): MyOption[A]
  //    = if (f(this.getOrElse(None))) this
  //      else None
}

object MyOption {

  case class MySome[+A](get: A) extends MyOption[A] {
    override def flatMap[B](f: A => MyOption[B]): MyOption[B] = f(get)

    override def orElse[B >: A](ob: => MyOption[B]): MyOption[B] = this

    override def filter(f: A => Boolean): MyOption[A] =
      if (f(get)) this
      else MyNone
  }

  case object MyNone extends MyOption[Nothing] {
    override def flatMap[B](f: Nothing => MyOption[B]): MyOption[B] = MyNone

    override def orElse[B >: Nothing](ob: => MyOption[B]): MyOption[B] = ob

    override def filter(f: Nothing => Boolean): MyOption[Nothing] = MyNone
  }

  def mean(xs: Seq[Double]): MyOption[Double] =
    if (xs.isEmpty) MyNone
    else MySome(xs.sum / xs.length)

  /**
   * EXERCISE 4.2
   */
  def variance(xs: Seq[Double]): MyOption[Double] = {
    import Math.pow
    mean(xs).flatMap(m => MySome(xs.map(x => pow(x - m, 2)).sum / xs.length))
  }

  def lift[A, B](f: A => B): MyOption[A] => MyOption[B] = x => x.map(f)

  /**
   * EXERCISE 4.3
   */
  def map2[A, B, C](a: MyOption[A], b: MyOption[B])(f: (A, B) => C): MyOption[C] = {
    //    val t0 = System.nanoTime()
    //    val res =
    (a, b) match {
      case (MySome(x), MySome(y)) => MySome(f(x, y))
      case _ => MyNone
    }

    //    val t1 = System.nanoTime()
    //    println(s"map2 = ${(t1 - t0) / 1e9} s")
    //    res
  }

  def insuranceRateQuote(age: Int, numberOfSpeedingTickets: Int): Double = ???

  def Try[A](a: => A): MyOption[A] =
    try MySome(a)
    catch {
      case e: Exception => MyNone
    }

  def parseInsuranceRateQuote(
                               age: String,
                               numberOfSpeedingTickets: String
                             ): MyOption[Double] = {
    val optAge: MyOption[Int] = Try {
      age.toInt
    }
    val optTickets: MyOption[Int] = Try {
      numberOfSpeedingTickets.toInt
    }
    map2(optAge, optTickets)(insuranceRateQuote)
  }

  /**
   * EXERCISE 4.4
   */
  def sequence[A](a: List[MyOption[A]]): MyOption[List[A]] = {
    a.foldRight(MySome(Nil): MyOption[List[A]])((x, acc) =>
      x match {
        case MyNone => MyNone
        case MySome(v) => acc map (l => v :: l)
      }
    )
  }

  /**
   * EXERCISE 4.5
   */
  def traverse[A, B](a: List[A])(f: A => MyOption[B]): MyOption[List[B]] = {
    a.foldRight[MyOption[List[B]]](MySome(Nil))((x, acc) =>
      f(x) match {
        case MyNone => MyNone
        case MySome(v) => acc map (l => v :: l)
      }
    )
  }

  def map2o[A, B, C](a: MyOption[A], b: MyOption[B])(f: (A, B) => C): MyOption[C] = {
    //    val t0 = System.nanoTime()
    //    val res =
    a flatMap (aa => b map (bb => f(aa, bb)))

    //    val t1 = System.nanoTime()
    //    println(s"map2o = ${(t1 - t0) / 1e9} s")
    //    res
  }

  def map2f[A, B, C](a: MyOption[A], b: MyOption[B])(f: (A, B) => C): MyOption[C] = {
    //    val t0 = System.nanoTime()
    //    val res =
    for {
      aa <- a
      bb <- b
    } yield f(aa, bb)

    //    val t1 = System.nanoTime()
    //    println(s"map2f = ${(t1 - t0) / 1e9} s")
    //    res
  }
}
