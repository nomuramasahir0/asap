/*
 * Copyright (c) 2018 Masahiro Nomura
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy of
 * this software and associated documentation files (the "Software"), to deal in
 * the Software without restriction, including without limitation the rights to
 * use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of
 * the Software, and to permit persons to whom the Software is furnished to do so,
 * subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all
 * copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS
 * FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR
 * COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
 * IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
 * CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 */

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

      if (stopCondition((newEvalCnt, min(fvals)))) {
        (newEvalCnt, sortedPop.X(*, 0).underlying)
      } else {
        optimize(strategy.update(sortedPop, sortedFvals), newEvalCnt)
      }
    }
    optimize(strategy, 0)
  }
}

