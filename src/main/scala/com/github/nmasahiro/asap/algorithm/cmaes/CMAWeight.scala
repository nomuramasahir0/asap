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

package com.github.nmasahiro.asap.algorithm.cmaes

import breeze.linalg.{DenseVector, min, sum}
import breeze.numerics.log


sealed trait CMAWeightType
case class CMAWeightActive() extends CMAWeightType
case class CMAWeightNormal() extends CMAWeightType

/**
  * The CMA Evolution Strategy: A Tutorial
  * Nikolaus Hansen
  * https://arxiv.org/abs/1604.00772 (2016)
  */
object CMAWeightActive {

  def getMueff(mu: Int, lambda: Int): Double = {
    val wPrime = (0 until lambda).map(i => log((lambda + 1.0) / 2.0) - log(i + 1.0))
    val mueff = math.pow(sum((0 until mu).map(i => wPrime(i))), 2) / sum((0 until mu).map(i => math.pow(wPrime(i), 2)))
    mueff
  }

  def getWeight(mu: Int, lambda: Int, dim: Int, c1: Double, cmu: Double): IndexedSeq[Double] = {
    val wPrime = (0 until lambda).map(i => log((lambda + 1.0) / 2.0) - log(i + 1.0))
    val mueff = math.pow(sum((0 until mu).map(i => wPrime(i))), 2) / sum((0 until mu).map(i => math.pow(wPrime(i), 2)))
    val mueffMinus = math.pow(sum((mu until lambda).map(i => wPrime(i))), 2) / sum((mu until lambda).map(i => math.pow(wPrime(i), 2)))
    val alphaMuMinus = 1 + c1 / cmu
    val alphaMueffMinus = 1 + (2.0 * mueffMinus) / (mueff + 2.0)
    val alphaPosdefMinus = (1 - c1 - cmu) / (dim * cmu)
    val wPrimePositiveSum = sum(wPrime.filter(_ >= 0.0))
    val wPrimeNegativeSum = sum(wPrime.filter(_ < 0.0).map(_.abs))
    val weights = wPrime.map { w =>
      if (w >= 0) 1 / wPrimePositiveSum * w
      else min(alphaMuMinus, alphaMueffMinus, alphaPosdefMinus) / wPrimeNegativeSum * w
    }
    weights
  }
}

/**
  * The CMA Evolution Strategy: A Comparing Review
  * Nikolaus Hansen
  * Towards a New Evolutionary Computation: Advances in the Estimation of Distribution Algorithms,
  * pp.75-102 (2006)
  */
object CMAWeightNormal {

  def getMueff(mu: Int, lambda: Int): Double = {
    val w = DenseVector.fill(mu)(math.log(mu + 1.0)) - DenseVector((0 until mu).map(v => math.log(v + 1.0)).toArray)
    val weights: DenseVector[Double] = DenseVector.vertcat(w / sum(w), DenseVector.zeros[Double](lambda - mu))
    1.0 / sum(weights *:* weights)
  }

  def getWeight(mu: Int, lambda: Int): IndexedSeq[Double] = {
    val w = DenseVector.fill(mu)(math.log(mu + 1.0)) - DenseVector((0 until mu).map(v => math.log(v + 1.0)).toArray)
    val weights: DenseVector[Double] = DenseVector.vertcat(w / sum(w), DenseVector.zeros[Double](lambda - mu))
    weights.toArray.toIndexedSeq
  }

}

