package com.github.nmasahiro.asap.algorithm

import breeze.linalg._
import com.github.nmasahiro.asap.util.StopCondition

import scala.annotation.tailrec

case class StrategyDriver(private val f: PartialFunction[DenseMatrix[Double], DenseVector[Double]]) {

  def optimize(strategy: Strategy, stopCondition: StopCondition): (Int, DenseVector[Double]) = {

    @tailrec
    def optimize(strategy: Strategy, evalCnt: Int): (Int, DenseVector[Double]) = {

      val pop = strategy.sampling

      val fvals = f(pop.X)

      val newEvalCnt = evalCnt + strategy.getLambda

      val (sortedPop, sortedFvals) = strategy.sorted(pop, fvals)
//      println(sortedFvals(0))

      if (stopCondition(newEvalCnt, min(fvals))) {
        (newEvalCnt, sortedPop.X(*, 0).underlying)
      } else {
        optimize(strategy.update(sortedPop, sortedFvals), newEvalCnt)
      }
    }
    optimize(strategy, 0)
  }
}

