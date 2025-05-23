package edu.neu.coe.csye7200.fp

import scala.concurrent._
import scala.language.postfixOps

/**
  * This case class is basically a Future-logger such that when the function has finished executing,
  * we print a comment on the error stream.
  *
  * There are two methods:
  * * the apply method which converts Seq[X] to Future[X] and
  * * the sequence method which converts Seq[Future[X] into Future[X]
  *
  * @param f    the function to apply to apply's input parameter
  * @param name the name of the function
  * @tparam X the underlying type
  */
case class Async[X](f: Seq[X] => X, name: String)(implicit executor: ExecutionContext) extends (Seq[X] => Future[X]) {
  def apply(xs: Seq[X]): Future[X] = Future {
    val x = f(xs)
    System.err.println(s"sequence starting ${xs.head} has $name $x")
    x
  }

  def sequence(xfs: Seq[Future[X]]): Future[X] = for (xs <- Future.sequence(xfs)) yield {
    val x = f(xs)
    System.err.println(s"sequence of futures has $name $x")
    x
  }
}

/**
  * Created by scalaprof on 2/17/17.
  */
object FutureExercise extends App {
  def integers(i: Int, n: Int): LazyList[BigInt] = LazyList.from(i).map(BigInt(_)) take n

  import scala.concurrent.ExecutionContext.Implicits.global
  import scala.concurrent.duration._

  val e = Async[BigInt](xs => xs.sum, "sum")
  private val chunk = 10000 // Try it first with chunk = 10000 and build up to 1000000
  private val xfs = for (i <- 0 to 9) yield e(integers(i * chunk + 1, chunk))
  private val xf = e.sequence(xfs)
  xf foreach { x => println(s"Sum: $x") }
  private val c10 = chunk * 10
  private val expected = xf filter (_ == BigInt((1L + c10) * c10 / 2))
  expected onComplete {
    case scala.util.Success(value) => println(value)
    case scala.util.Failure(x) => System.err.println(x.getLocalizedMessage)
  }
  Await.ready(expected, 10000 milli)
  println("Goodbye")
}

object FutureExercise2 extends App {
  import scala.concurrent.ExecutionContext.Implicits.global
  val chunk = 10000 // Try it first with chunk = 10000 and build up to 1000000
  def integers(i: Int, n: Int): LazyList[Int] = LazyList.from(i) take n
  def sum[N : Numeric](is: LazyList[N]): BigInt = is.foldLeft(BigInt(0))(_+implicitly[Numeric[N]].toLong(_))
  def asyncSum(is: LazyList[Int]): Future[BigInt] = Future {val x = sum(is); System.err.println(s"${is.head} is done with sum $x"); x}
  val xfs = for (i <- 0 to 10) yield asyncSum(integers(i * chunk, chunk))
  val xsf = Future.sequence(xfs)
  val xf: Future[BigInt] = for (ls <- xsf) yield ls.sum
}