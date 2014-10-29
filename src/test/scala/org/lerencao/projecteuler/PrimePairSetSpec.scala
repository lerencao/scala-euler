package org.lerencao.projecteuler

import org.scalatest._


abstract class UnitSpec extends FlatSpec with Matchers with OptionValues with Inside with Inspectors

class PrimePairSetSpec extends UnitSpec {
  "prime stream" should "generate prime numbers correctly" in {
    Prime.primes.take(10).toList shouldBe List(2, 3, 5, 7, 11, 13, 17, 19, 23, 29)
  }

  "prime pairing" should "match correctly" in {
    val pair = List(3, 7, 109)
    PrimePairSet.canBePairs(3, Nil) shouldBe true
    PrimePairSet.canBePairs(7, List(3)) shouldBe true
    PrimePairSet.canBePairs(109, List(7)) shouldBe true
    PrimePairSet.canBePairs(673, List(109)) shouldBe true
  }

  "pair set" should "work" in {
    PrimePairSet().length shouldBe 5
  }
}
