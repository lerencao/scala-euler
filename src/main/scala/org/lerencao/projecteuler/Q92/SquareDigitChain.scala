package org.lerencao.projecteuler.Q92

import scala.collection.mutable

object SquareDigitChain {
  private def sumDigitsSquare(number: Int): Int = {
    var (num, digitSquare) = (number, 0)
    while (num != 0) {
      val remainder = num % 10
      digitSquare += remainder * remainder
      num = num / 10
    }
    digitSquare
  }

  def apply(startNumber: Int): Stream[Int] = {
    require(startNumber > 0, "start number should be greater than 0")
    val digitSquare = sumDigitsSquare(startNumber)
    digitSquare #:: apply(digitSquare)
  }
}

object Solution {
  def apply(upperBound: Int): Int = {
    // except the start number, the biggest chained number is digitLength * 9^2
    val cache = Array.fill(upperBound.toString.length * 81 + 1) { 0 }
    var count = 0 // count for 89

    for (n <- 1 until upperBound) {
      val chains = SquareDigitChain(n)

      val beforeTerminate = chains.takeWhile(c => cache(c) == 0 && c != 1 && c != 89)
      val terminator = chains(beforeTerminate.length)

      if (cache(terminator) == 89 || terminator == 89) count += 1
      for (m <- beforeTerminate)
        cache(m) = if (cache(terminator) == 0) terminator else cache(terminator)
    }

    count
  }

  /** The approach is based on ceebo53's comment on project euler.
    * However, it is only fit for the cases that the upper bound is 10^n.
    *
    * > There's actually no need to store huge lists or to have to cycle through each number from 1 to 10^7.
    *
    * > After a single iteration of the "sum squares of digits" step the number is guaranteed to be less than or equal to 9^2*7 = 567.
    *
    * > Now for each number n <= 567 that ends its cycle with 89 the question becomes:
    * > "How many numbers below 10^7 have the sum of the squares of their digits equal to n?"

    * > Let the number of ways of writing n as the sum of k squares be f(n,k).
    * > Then f can be computed by the recurrence relation:
    * > f(n,k) = f(n-0^2,k-1) + f(n-1^2,k-1) + f(n-2^2,k-1) + .... + f(n-9^2,k-1)

    * > Base cases:
    * > f(n,k) = 0 if n<0
    * > f(n,0) = 0 if n>0
    * > f(0,0) = 1
    * @param maxDigitsLength the digit length of the upper bound. Notice: the upper bound must be 10^n
    * @return the number whose square digit chain end with 89.
    */
  def alternative(maxDigitsLength: Int): Int = {
    val cache = mutable.HashMap[(Int, Int), Int]()
    // init the cache
    cache.put((0, 0), 1)
    for (n <- 1 until (maxDigitsLength * 81 + 1)) cache.put((n, 0), 0)

    for {
      k <- 1 until (maxDigitsLength + 1)
      n <- 0 until (maxDigitsLength * 81 + 1)
    } {
      val t = (0 until 10).map({ digit =>
        if (n - digit * digit < 0) 0
        else cache((n - digit * digit, k - 1))
      }).sum
      cache.put((n, k), t)
    }

    val ns = Range(1, maxDigitsLength * 81 + 1).filter { i =>
      val chains = SquareDigitChain(i)
      val pos = chains.takeWhile(c => c != 1 && c != 89).length
      chains(pos) == 89
    }
    ns.map { n => cache((n, maxDigitsLength)) }.sum
  }

}
