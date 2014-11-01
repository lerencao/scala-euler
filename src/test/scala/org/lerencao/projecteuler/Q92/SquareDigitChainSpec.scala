package org.lerencao.projecteuler.Q92

import org.lerencao.projecteuler.utils.UnitSpec

class SquareDigitChainSpec extends UnitSpec {
  "SquareDigitChain" should "return proper series" in {
    SquareDigitChain(44).takeWhile(_ != 1).toList shouldBe List(32, 13, 10)
    SquareDigitChain(85).takeWhile(_ != 89).toList shouldBe Nil
  }
  "runAnswer" should "return correct answer" in {
    println(SquareDigitChain.runAnswer(10000000))
  }
}
