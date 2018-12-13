package com.github.nmasahiro.asap.algorithm.dxnes

import breeze.linalg._
import breeze.linalg.eigSym.EigSym
import breeze.numerics.{exp, log, sqrt}
import com.github.nmasahiro.asap.algorithm.{Population, Strategy}
import breeze.stats.distributions
import breeze.stats.distributions.RandBasis

class DXNES private[dxnes](iteration: Int,
                           lambda: Int,
                           dim: Int,
                           ps: DenseVector[Double],
                           B: DenseMatrix[Double],
                           sigma: Double,
                           mean: DenseVector[Double])(implicit randBasis: RandBasis) extends Strategy {

  override def getLambda: Int = lambda

  private val weightsRankHat: DenseVector[Double] =
    (DenseVector.fill(lambda)(math.log(lambda / 2 + 1)) - DenseVector((0 until lambda).map(v => math.log(v + 1.0)).toArray))
      .map(max(0.0, _))
  private val weightsRank = weightsRankHat / sum(weightsRankHat) - 1.0 / lambda

  private val mueff = 1 / sum(weightsRank.map(_ + 1.0 / lambda).map(math.pow(_, 2)))

  private val csig = (mueff + 2.0) / (dim + mueff + 5.0) / sqrt(dim)

  private val chiN = sqrt(dim) * (1.0 - 1.0 / (4.0 * dim) + 1.0 / (21.0 * dim * dim))

  private val alpha = (0.9 + 0.15 * log(dim)) * min(1.0, lambda / dim.toDouble)

  private def wDistHat(z: DenseVector[Double]): Double = exp(alpha * norm(z))

  private def getEtaSigmaMove = 1.0

  private def getEtaSigmaStag = 0.5 * (1.0 + lambda / (lambda + 2.0 * dim))

  private def getEtaSigmaConv = 1.0 + lambda / (lambda + 2.0 * dim)

  private def getEtaBMove = (lambda + 2.0 * dim) / (lambda + 2.0 * dim * dim + 100) * min(1.0, sqrt(lambda / dim.toDouble))

  private def getEtaBStag = lambda / (lambda + (2.0 * dim * dim) + 100)

  private def getEtaBConv = getEtaBStag

  private val g = distributions.Gaussian(0, 1)

  def sampling: Population = {
    val Zhalf = DenseMatrix((0 until lambda / 2).map(_ => g.samplesVector(dim).toArray): _*).t
    // Z: dim x lambda
    val Z = DenseMatrix.horzcat(Zhalf, -Zhalf)
    // Y: dim x lambda
    val Y = B * Z
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

  def update(pop: Population, fvals: DenseVector[Double]): DXNES = {

    val wRankZMatrix = pop.Z * diag(weightsRank)
    val wRankZ = sum(wRankZMatrix(*, ::))

    val psN: DenseVector[Double] = (1.0 - csig) * ps + sqrt(csig * (2.0 - csig) * mueff) * wRankZ

    val weightsDist: DenseVector[Double] = {
      val wRankDistHat = (0 until lambda).map(i => weightsRankHat(i) * wDistHat(pop.Z(::, i)))
      val wSum = sum(wRankDistHat)
      DenseVector(wRankDistHat.map(w => w / wSum - 1.0 / lambda).toArray)
    }

    val weights = if (norm(psN) >= chiN) weightsDist else weightsRank

    val etaSigma = if (norm(psN) >= chiN) getEtaSigmaMove
    else if (norm(psN) >= 0.1 * chiN) getEtaSigmaStag
    else getEtaSigmaConv

    val etaB = if (norm(psN) >= chiN) getEtaBMove
    else if (norm(psN) >= 0.1 * chiN) getEtaBStag
    else getEtaBConv

    val wzMatrix = pop.Z * diag(weights)
    val Gdelta = sum(wzMatrix(*, ::))

    val GM = (0 until lambda).map { i => weights(i) * (pop.Z(::, i) * pop.Z(::, i).t - DenseMatrix.eye[Double](dim)) }
      .foldLeft(DenseMatrix.zeros[Double](dim, dim))(_ + _)

    val Gsigma = trace(GM) / dim.toDouble

    val preGB = GM - Gsigma * DenseMatrix.eye[Double](dim)
    val GB = (preGB + preGB.t) / 2.0

    val meanN: DenseVector[Double] = mean + (sigma * B * Gdelta)

    val sigmaN = sigma * exp(etaSigma * Gsigma / 2.0)

    val EigSym(nDsquare, nU) = eigSym(etaB * GB / 2.0)
    val nB = B * (nU * diag(nDsquare.map(exp(_))) * nU.t)

    new DXNES(
      iteration + 1,
      lambda,
      dim,
      psN,
      nB,
      sigmaN,
      meanN
    )
  }
}

object DXNES {

  def apply(lambda: Int, initialM: DenseVector[Double], initialSigma: Double)(implicit randBasis: RandBasis): DXNES = {
    new DXNES(1, lambda, initialM.length,
      DenseVector.zeros[Double](initialM.length),
      DenseMatrix.eye[Double](initialM.length),
      initialSigma,
      initialM
    )
  }

}
