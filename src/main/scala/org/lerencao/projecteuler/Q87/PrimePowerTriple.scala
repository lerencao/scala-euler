package org.lerencao.projecteuler.Q87

import scala.collection.mutable.{Set => MutableSet}
import org.lerencao.projecteuler.utils.Prime

object PrimePowerTriple {
  /**
   * 1. For each pow, calculate the biggest possible prime number. We can get: 7069, 367, 83.
   * 2. Then do nested loop and record the result below the upper bound(which is 50_000_000).
   * That's it!
   * @param upperBound upper bound
   * @return count of numbers that can be expressed as the sum of a prime square, prime cube, and prime fourth power
   */
  def apply(upperBound: Int): Int = {
    val cache: Array[Map[Int, Int]] = Array(2, 3, 4).map { pow =>
      Prime.primes.takeWhile(_ < math.pow(upperBound, 1.0 / pow)).map(p => (p, math.pow(p, pow).toInt)).toMap
    }
    val values = MutableSet[Int]()

    for {
      a <- Prime.primes.takeWhile(_ < math.pow(upperBound, 1.0 / 2))
      b <- Prime.primes.takeWhile(_ < math.pow(upperBound, 1.0 / 3))
      c <- Prime.primes.takeWhile(_ < math.pow(upperBound, 1.0 / 4))
    } {
      val t = cache(0)(a) + cache(1)(b) + cache(2)(c)
      if (t < upperBound) values.add(t)
    }

    values.size
  }
}
