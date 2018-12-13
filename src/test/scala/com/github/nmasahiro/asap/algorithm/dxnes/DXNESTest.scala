package com.github.nmasahiro.asap.algorithm.dxnes

import breeze.linalg._
import breeze.stats.distributions.{RandBasis, ThreadLocalRandomGenerator}
import com.github.nmasahiro.asap.algorithm.{ParallelBenchmark, StrategyDriver}
import org.scalatest.{FunSuite, Matchers}
import com.github.nmasahiro.asap.util._
import org.apache.commons.math3.random.MersenneTwister

class DXNESTest extends FunSuite with Matchers {

  test("DX-NES with d=40 k-tablet Function:") {

    val driver = StrategyDriver(ParallelBenchmark.ktablet)

    val seed = 10
    implicit val randBasis: RandBasis = new RandBasis(new ThreadLocalRandomGenerator(new MersenneTwister(seed)))

    val dim = 40
    val lambda = 40
    val initialM = 3.0 * DenseVector.ones[Double](dim)
    val initialSigma = 2.0
    val dxnes = DXNES(lambda, initialM, initialSigma)

    val successFval = 1e-12
    val finishEvalCnt = (5 * dim * 1e4).toInt

    val (evalCnt, bestX) = driver.optimize(
      dxnes,
      fvalBestReached(successFval) orElse
        evalCntReached(finishEvalCnt) orElse
        proceed
    )

    println(s"evalCnt:$evalCnt, bestX:$bestX")
    evalCnt shouldBe (48 * 1e3.toInt +- (5 * 1e3.toInt))

  }

  test("DX-NES with d=40 RosenbrockChain Function:") {

    val driver = StrategyDriver(ParallelBenchmark.rosenbrock)

    val seed = 10
    implicit val randBasis: RandBasis = new RandBasis(new ThreadLocalRandomGenerator(new MersenneTwister(seed)))

    val dim = 40
    val lambda = 32
    val initialM = DenseVector.zeros[Double](dim)
    val initialSigma = 0.5
    val dxnes = DXNES(lambda, initialM, initialSigma)

    val successFval = 1e-12
    val finishEvalCnt = (5 * dim * 1e4).toInt

    val (evalCnt, bestX) = driver.optimize(
      dxnes,
      fvalBestReached(successFval) orElse
        evalCntReached(finishEvalCnt) orElse
        proceed
    )

    println(s"evalCnt:$evalCnt, bestX:$bestX")
    evalCnt shouldBe ((93.2 * 1e3).toInt +- (6 * 1e3.toInt))

  }

}
