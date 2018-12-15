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

package com.github.nmasahiro.asap

import breeze.linalg._

package object util {

  type StopCondition = PartialFunction[(Int, Double), Boolean]

  val proceed: StopCondition = {
    case (_, _) => false
  }

  def fvalBestReached(fvalMin: Double): StopCondition = {
    case (_, fvalBest: Double) if fvalBest < fvalMin => true
  }

  def evalCntReached(maxEvalCnt: Int): StopCondition = {
    case (evalCnt: Int, _) if evalCnt > maxEvalCnt => true
  }

  case class SimpleObjectiveFunction(objFunction: PartialFunction[DenseVector[Double], Double]) extends PartialFunction[DenseMatrix[Double], DenseVector[Double]] {
    def apply(pop: DenseMatrix[Double]): DenseVector[Double] = {
      pop(::, *).map {
        x => objFunction.apply(x)
      }.inner
    }
    def isDefinedAt(pop: DenseMatrix[Double]): Boolean = {
      pop(::, *).map { x =>
        objFunction.isDefinedAt(x)
      }.inner.reduce((a, b) => a && b)
    }
  }

  case class ParallelObjectiveFunction(objFunction: PartialFunction[DenseVector[Double], Double]) extends PartialFunction[DenseMatrix[Double], DenseVector[Double]] {
    def apply(pop: DenseMatrix[Double]): DenseVector[Double] = {
      DenseVector {
        pop(::, *).iterator.toSeq.par.map {
          x => objFunction.apply(x)
        }.toArray
      }
    }
    def isDefinedAt(pop: DenseMatrix[Double]): Boolean = {
      pop(::, *).iterator.toSeq.par.map(objFunction.isDefinedAt).reduce {
        (a, b) => a && b
      }
    }
  }

}
