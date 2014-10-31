package org.lerencao.projecteuler.utils

/**
 * prime number generator
 */
object Prime {
  private val odd: Stream[Int] = Stream.from(1, 2)
  def isPrime(n: Int): Boolean = primes.takeWhile(_ <= math.sqrt(n)).forall(n % _ != 0)
  val primes: Stream[Int] = 2 #:: odd.tail.filter(isPrime)
  // uncomment it to use sieve methods
  //  private def sieve(s: Stream[Int]): Stream[Int] = s.head #:: sieve(s.tail.filter(_ % s.head != 0))
  //  val primesSieve: Stream[Int] = 2 #:: sieve(odd.tail)
}
