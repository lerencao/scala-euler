package org.lerencao.projecteuler.Q92

object SquareDigitChain {
  def apply(startNumber: Int): Stream[Int] = {
    require(startNumber > 0, "start number should be greater than 0")
    var (num, digitSquare) = (startNumber, 0)
    while (num != 0) {
      val remainder = num % 10
      digitSquare += remainder * remainder
      num = num / 10
    }
    digitSquare #:: apply(digitSquare)
  }

  def runAnswer(upperBound: Int): Int = {
    // except the start number, the biggest chained number is digitLength * 9^2
    val cache = Array.fill(upperBound.toString.length * 81 + 1) { 0 }
    var count = 0 // count for 89

    for (n <- 1 until upperBound) {
      val chains = apply(n)

      val beforeTerminate = chains.takeWhile(c => cache(c) == 0 && c != 1 && c != 89)
      val terminator = chains(beforeTerminate.length)

      if (cache(terminator) == 89 || terminator == 89) count += 1
      for (m <- beforeTerminate)
        cache(m) = if (cache(terminator) == 0) terminator else cache(terminator)
    }

    count
  }
}
