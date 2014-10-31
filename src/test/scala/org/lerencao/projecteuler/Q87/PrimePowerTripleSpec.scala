package org.lerencao.projecteuler.Q87

import org.lerencao.projecteuler.utils.UnitSpec

class PrimePowerTripleSpec extends UnitSpec {
  "PrimePowerTriple" should "results in correct answer" in {
    PrimePowerTriple(50) shouldBe 4
    PrimePowerTriple(50000000) shouldBe 1097343
  }
}
