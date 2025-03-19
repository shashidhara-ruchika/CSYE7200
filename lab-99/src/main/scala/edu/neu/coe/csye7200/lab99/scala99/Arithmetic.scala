/*
 * Copyright (c) 2019. Phasmid Software
 */

package edu.neu.coe.csye7200.lab99.scala99

import scala.language.implicitConversions

class Arithmetic(val x: Int) {

  import Arithmetic._

  // P31
  def isPrime: Boolean = (x > 1) &&
      primes.takeWhile(_ <= Math.sqrt(x)).forall(x % _ != 0)

  // P33
  def isCoprimeTo(n: Int): Boolean = {
    gcd(x, n) == 1
  }
  // TO BE IMPLEMENTED

  // P34
  def totient: Int =
    // TO BE IMPLEMENTED
    (1 until x).count(_.isCoprimeTo(x))

  // P37
  def totientP37: Int = // SOLUTION
    primeFactorMultiplicity.foldLeft(1) { (r, f) =>
      f match {
        case (p, m) => r * (p - 1) * Math.pow(p, m - 1).toInt
      }
    }

  // P35 (amended by P36)
  def primeFactors: Seq[Int] = {
    val xs = (primes takeWhile (_ <= Math.sqrt(x))) to List
    xs filter (x % _ == 0)
  }

  // P36
  def primeFactorMultiplicity: Map[Int, Int] = {
    // TO BE IMPLEMENTED
    primeFactors.groupBy(identity).view.mapValues(_.size).toMap
  }

  // P40
  def goldbach: (Int, Int) =
    // TO BE IMPLEMENTED
  {
    require(x > 2 && x % 2 == 0, "Goldbach's conjecture applies only to even numbers greater than 2")
    primes.takeWhile(_ < x).find(p => (x - p).isPrime) match {
      case Some(p) => (p, x - p)
      case None => throw new IllegalArgumentException("Goldbach's conjecture failed (which is highly unlikely)")
    }
  }
}

object Arithmetic {
  implicit def int2S99Int(i: Int): Arithmetic = new Arithmetic(i)

  // P31
  lazy val primes: LazyList[Int] = 2 #:: LazyList.from(3, 2).filter(_.isPrime)

  // P32
  @scala.annotation.tailrec
  def gcd(m: Int, n: Int): Int = {
    if (n == 0) m else gcd(n, m % n)
  }

  // P39
  def listPrimesInRange(r: Range): Seq[Int] =
    // TO BE IMPLEMENTED
    r.filter(_.isPrime)


  // P41
  def printGoldbachList(r: Range): Unit = {
    // TO BE IMPLEMENTED
    r.filter(n => n > 2 && n % 2 == 0).foreach { n =>
      val (p1, p2) = n.goldbach
      println(s"$n = $p1 + $p2")
    }
  }

  // P41
  def printGoldbachListLimited(r: Range, limit: Int): Unit = {
    // TO BE IMPLEMENTED
    r.filter(n => n > 2 && n % 2 == 0).foreach { n =>
      val (p1, p2) = n.goldbach
      if (p1 > limit) println(s"$n = $p1 + $p2")
    }
  }

}