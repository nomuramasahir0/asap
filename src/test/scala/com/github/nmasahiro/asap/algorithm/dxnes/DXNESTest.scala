package com.github.nmasahiro.asap.algorithm.dxnes

import breeze.linalg._
import com.github.nmasahiro.asap.algorithm.StrategyDriver
import org.scalatest.{FunSuite, Matchers}
import com.github.nmasahiro.asap.util._

class DXNESTest extends FunSuite with Matchers {

  val sphere = ParallelObjectiveFunction ({
    case x: DenseVector[Double] =>
      (for (i <- 0 until x.length) yield {
        x(i) * x(i)
      }).sum
  })

  test("DX-NES with Sphere Function:") {

    val driver = StrategyDriver(sphere)

    val dim = 2
    val lambda = 6
    val initialM = DenseVector.zeros[Double](dim)
    val initialSigma = 2.0
    val dxnes = DXNES(lambda, initialM, initialSigma)

    val result = driver.optimize(
      dxnes,
      fvalBestReached(1e-5) orElse
      evalCntReached(500) orElse
      proceed
    )
    println(result)

  }

}
