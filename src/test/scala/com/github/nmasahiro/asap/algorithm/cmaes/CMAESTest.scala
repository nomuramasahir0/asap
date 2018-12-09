package com.github.nmasahiro.asap.algorithm.cmaes

import breeze.linalg._
import com.github.nmasahiro.asap.algorithm.StrategyDriver
import org.scalatest.{FunSuite, Matchers}
import com.github.nmasahiro.asap.util._

class CMAESTest extends FunSuite with Matchers {

  val rosenbrock = ParallelObjectiveFunction ({
    case x: DenseVector[Double] =>
      (for (i <- 0 until x.length - 1) yield {
        100 * (math.pow(x(i), 2) - x(i + 1)) * (math.pow(x(i), 2) - x(i + 1)) +
          math.pow(x(i) - 1.0, 2)
      }).sum
  })

  val sphere = ParallelObjectiveFunction ({
    case x: DenseVector[Double] =>
      (for (i <- 0 until x.length) yield {
        x(i) * x(i)
      }).sum
  })


  test("CMA-ES with RosenbrockChain Function:") {

//    val driver = StrategyDriver(rosenbrock)
    val driver = StrategyDriver(sphere)

    val dim = 2
    val lambda = 6
    val initialM = DenseVector.zeros[Double](dim)
    val initialSigma = 0.5
    val cmaes = CMAES(lambda, initialM, initialSigma)

    val result = driver.optimize(
      cmaes,
      fvalBestReached(1e-5) orElse
        evalCntReached(500) orElse
        proceed
    )

    println(result)

  }

}
