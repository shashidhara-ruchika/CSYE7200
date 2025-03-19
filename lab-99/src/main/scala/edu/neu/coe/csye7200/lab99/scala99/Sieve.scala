package edu.neu.coe.csye7200.lab99.scala99

object Sieve extends App {
    val primes: LazyList[Int] = 2 #:: LazyList.from(3, 2).filter(isPrime(_))

    def isPrime(x: BigInt): Boolean = (x > 1) && primes.takeWhile(_ <= Math.sqrt(x.toDouble)).forall(x % _ != 0)

    println(primes take 100_000 to List)
}