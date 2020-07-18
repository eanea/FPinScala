package Part2.Chapter7

import java.util.concurrent.Executors

object Chapter7 extends App {
  import Par._

  def sum(ints: IndexedSeq[Int]): Par[Int] =
    if (ints.length <= 1)
      Par.unit(ints.headOption getOrElse 0)
    else {
      val (l, r) = ints.splitAt(ints.length / 2)
      Par.map2(Par.fork(sum(l)), Par.fork(sum(r)))(_ + _)
    }

  def max(ints: IndexedSeq[Int]): Par[Int] =
    if (ints.length <= 1)
      Par.unit(ints.headOption getOrElse Int.MinValue)
    else {
      val (l, r) = ints.splitAt(ints.length / 2)
      Par.map2(Par.fork(max(l)), Par.fork(max(r)))(_ max _)
    }

  def totalNumber(l: List[String]): Par[Int] = {
    val lpi = l.map(asyncF(x => x.split(" ").length))
    lpi.foldRight(unit(0))((p, acc) => map2(p, acc)(_ + _))
  }

  val sumList         = max(IndexedSeq(1, 2, 3, 4, 5))
  val executorService = Executors.newScheduledThreadPool(4)
  val result          = Par.run(executorService)(sumList)

  println(result.isDone)
  println(result.get())
}
