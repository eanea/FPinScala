package Part2.Chapter8

import Part2.Chapter8.Prop.{FailedCase, SuccessCount}

sealed trait Result {
  def isFalsified: Boolean
}
object Result {
  case object Passed extends Result {
    def isFalsified = false
  }

  case class Falsified(failure: FailedCase, successes: SuccessCount) extends Result {
    def isFalsified = true
  }

  case object Proved extends Result {
    override def isFalsified: Boolean = false
  }
}