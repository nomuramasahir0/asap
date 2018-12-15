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

import breeze.linalg._
import breeze.numerics.{pow, sqrt}
import breeze.linalg.eigSym.EigSym
import breeze.stats.distributions
import breeze.stats.distributions.RandBasis
import com.github.nmasahiro.asap.algorithm.{Population, Strategy}


class CMAES private[cmaes](iteration: Int,
                           lambda: Int,
                           dim: Int,
                           ps: DenseVector[Double],
                           pc: DenseVector[Double],
                           B: DenseMatrix[Double],
                           C: DenseMatrix[Double],
                           D: DenseVector[Double],
                           sigma: Double,
                           mean: DenseVector[Double],
                           weightType: CMAWeightType)(implicit randBasis: RandBasis) extends Strategy {

  override def getLambda: Int = lambda

  private val mu = math.floor(lambda / 2.toDouble).toInt

  private val mueff = weightType match {
    case CMAWeightNormal() => CMAWeightNormal.getMueff(mu, lambda)
    case CMAWeightActive() => CMAWeightActive.getMueff(mu, lambda)
  }

  private val csig = (mueff + 2) / (dim + mueff + 5)

  private val cc = (4.0 + mueff / dim) / (dim + 4.0 + 2.0 * mueff / dim)

  private val c1 = 2.0 / (math.pow(dim + 1.3, 2.0) + mueff)

  private val cmu = min(1.0 - c1, 2.0 * (mueff - 2.0 + 1.0 / mueff) / (math.pow(dim + 2, 2) + mueff))

  private val chiN = math.sqrt(dim) * (1.0 - 1.0 / (4.0 * dim) + 1.0 / (21.0 * dim * dim))

  private val damps = 1.0 + 2 * max(0.0, math.sqrt((mueff - 1.0) / (dim + 1.0)) - 1.0) + csig

  private val weights = weightType match {
    case CMAWeightNormal() => CMAWeightNormal.getWeight(mu, lambda)
    case CMAWeightActive() => CMAWeightActive.getWeight(mu, lambda, dim, c1, cmu)
  }

  private val g = distributions.Gaussian(0, 1)

  def sampling: Population = {
    // Z: dim x lambda
    val Z = DenseMatrix((0 until lambda).map(_ => g.samplesVector(dim).toArray): _*).t
    // Y: dim x lambda
    val Y = B * diag(D) * Z
    // X: dim x lambda
    val Ysigma = Y *:* sigma
    val X = Ysigma(::, *) + mean

    Population(Z, Y, X)
  }

  def sorted(pop: Population, fvals: DenseVector[Double]): (Population, DenseVector[Double]) = {
    val argFvals = argsort(fvals)

    def sort(m: DenseMatrix[Double]): DenseMatrix[Double] =
      DenseMatrix((0 until lambda).map(i => m(::, argFvals(i)).toArray): _*).t

    (Population(sort(pop.Z), sort(pop.Y), sort(pop.X)),
      DenseVector(fvals.toArray.sorted))
  }

  def update(pop: Population, fvals: DenseVector[Double]): CMAES = {

    val weightsDiag = diag(DenseVector(weights.toArray))
    val wYMatrix = pop.Y(::, 0 until mu) * weightsDiag(0 until mu, 0 until mu)
    val wY: DenseVector[Double] = sum(wYMatrix(*, ::))

    val meanN = mean + wY *:* sigma

    val invSqrtC = B * diag(D ^:^ (-1.0)) * B.t

    val psN: DenseVector[Double] = (1.0 - csig) * ps + sqrt(csig * (2.0 - csig) * mueff) * invSqrtC * wY

    val sigmaN = sigma * math.exp((csig / damps) * ((norm(psN) / chiN) - 1.0))

    val hsig =
      if (norm(psN) / math.sqrt(1 - pow(1.0 - (1.0 - csig), 2.0 * (iteration + 1))) / chiN < 1.4 + 2.0 / (dim + 1.0)) 1.0
      else 0.0
    val deltaHsig = (1.0 - hsig) * cc * (2.0 - cc)

    val pcN: DenseVector[Double] = (1.0 - cc) * pc + hsig * sqrt(cc * (2.0 - cc) * mueff) * wY

    val weightsRankMu = (0 until lambda).map { i =>
      if (weights(i) >= 0) weights(i)
      else weights(i) * dim / math.pow(norm(invSqrtC * pop.Y(::, i)), 2)
    }

    val rankOne = pcN * pcN.t

    val rankMu = (0 until lambda).map { i => weightsRankMu(i) * pop.Y(::, i) * pop.Y(::, i).t }
      .foldLeft(DenseMatrix.zeros[Double](dim, dim))(_ + _)

    val nC: DenseMatrix[Double] = (1 + c1 * deltaHsig - c1 - cmu * sum(weights)) * C +
      c1 * rankOne +
      cmu * rankMu

    val nCSym = (nC + nC.t) / 2.0

    val EigSym(nDSquare, nB) = eigSym(nCSym)

    val nD = sqrt(nDSquare)

    new CMAES(
      iteration + 1,
      lambda,
      dim,
      psN,
      pcN,
      nB,
      nC,
      nD,
      sigmaN,
      meanN,
      weightType
    )
  }
}

object CMAES {

  def apply(lambda: Int, initialM: DenseVector[Double], initialSigma: Double)(implicit randBasis: RandBasis): CMAES =
    apply(lambda, initialM, initialSigma, CMAWeightActive())(randBasis)

  def apply(lambda: Int, initialM: DenseVector[Double], initialSigma: Double, weightType: CMAWeightType)
           (implicit randBasis: RandBasis): CMAES = {
    new CMAES(1, lambda, initialM.length,
      DenseVector.zeros[Double](initialM.length),
      DenseVector.zeros[Double](initialM.length),
      DenseMatrix.eye[Double](initialM.length),
      DenseMatrix.eye[Double](initialM.length),
      DenseVector.ones[Double](initialM.length),
      initialSigma,
      initialM,
      weightType)
  }
}
