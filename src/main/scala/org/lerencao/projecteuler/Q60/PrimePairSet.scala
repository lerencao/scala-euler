package org.lerencao.projecteuler.Q60

import org.lerencao.projecteuler.utils.Prime

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future}


object PrimePairSet {
  import scala.collection.mutable.{HashSet => MutableSet}

  private def canBePair(p1: Int, p2: Int): Boolean = {
    Prime.isPrime(s"$p1$p2".toInt) && Prime.isPrime(s"$p2$p1".toInt)
  }

  def canBePairs(p: Int, pair: List[Int]): Boolean = {
    pair.forall(canBePair(p, _))
  }
  def apply(): List[Int] = {
    val pairs: List[MutableSet[List[Int]]] = List(
      MutableSet[List[Int]](),
      MutableSet[List[Int]](),
      MutableSet[List[Int]](),
      MutableSet[List[Int]](),
      MutableSet[List[Int]]()
    )

    Prime.primes.takeWhile(_ => pairs.last.isEmpty).foreach { prime =>
      val upgradings: List[Future[Set[List[Int]]]] = pairs.dropRight(1).map { level =>
        Future { level.filter(canBePairs(prime, _)).map(prime +: _).toSet }
      }

      val allDone: Future[List[Set[List[Int]]]] = Future.sequence(upgradings)

      val upgrades = Await.result(allDone, Duration.Inf)

      pairs.tail.zip(upgrades).foreach(tuple => tuple._1 ++= tuple._2)
      pairs.head.add(List(prime))
    }

    pairs.last.head
  }
}
