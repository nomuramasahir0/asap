package com.github.nmasahiro.asap.algorithm.fmnes

import breeze.linalg._
import breeze.linalg.eigSym.EigSym
import breeze.numerics.{exp, sqrt, tanh}
import com.github.nmasahiro.asap.algorithm.{Population, Strategy}
import breeze.stats.distributions

class FMNES private[fmnes](iteration: Int,
                           lambda: Int,
                           dim: Int,
                           ps: DenseVector[Double],
                           pc: DenseVector[Double],
                           B: DenseMatrix[Double],
                           sigma: Double,
                           mean: DenseVector[Double],
                           gamma: Double,
                           hInv: Double) extends Strategy {

  override def getLambda: Int = lambda

  private val weightsRankHat: DenseVector[Double] =
    (DenseVector.fill(lambda)(math.log(lambda / 2 + 1)) - DenseVector((0 until lambda).map(v => math.log(v + 1.0)).toArray))
      .map(max(0.0, _))
  private val weightsRank = weightsRankHat / sum(weightsRankHat) - 1.0 / lambda

  private val mueff = 1 / sum(weightsRank.map(_ + 1.0 / lambda).map(math.pow(_, 2)))

  private val csig = (mueff + 2.0) / (dim + mueff + 5.0) / sqrt(dim)

  private val cc = (4.0 + mueff / dim) / (dim + 4.0 + 2.0 * mueff / dim)

  private val c1 = 2.0 / (math.pow(dim + 1.3, 2) + mueff)

  private def getC1(lambdaF: Int): Double = c1 * (lambdaF / lambda.toDouble)

  private val chiN = sqrt(dim) * (1.0 - 1.0 / (4.0 * dim) + 1.0 / (21.0 * dim * dim))

  private val etaM = 1.0

  // eiadx-nes
  private val cGamma = 1.0 / (3.0 * (dim - 1.0))
  private val dGamma = min(1.0, dim.toDouble / lambda)

  // distance weight
  private def getAlphaDist(lambdaF: Int) = hInv * min(1.0, sqrt(lambda / dim.toDouble)) * sqrt(lambdaF / lambda.toDouble)

  private def wDistHat(z: DenseVector[Double], lambdaF: Int): Double = exp(getAlphaDist(lambdaF) * norm(z))

  private def getEtaSigmaMove = 1.0

  private def getEtaSigmaStag(lambdaF: Int) = tanh((0.024 * lambdaF + 0.7 * dim + 20) / (dim + 12.0))

  private def getEtaSigmaConv(lambdaF: Int) = 2 * tanh((0.025 * lambdaF + 0.75 * dim + 10) / (dim + 4.0))

  private val preCMu = 120.0 * dim / (47.0 * dim * dim + 6400.0)

  private def getCMu(lambdaF: Int) = preCMu * math.tanh(0.02 * lambdaF)

  private val g = distributions.Gaussian(0, 1)

  def expm(m: DenseMatrix[Double]): DenseMatrix[Double] = {
    val EigSym(dVec, uMat) = eigSym(m)
    uMat * diag(dVec.map(exp(_))) * uMat.t
  }

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
    val threFval = max(fvals.toArray.filter(_ < Double.MaxValue))

    val scaledFvals = DenseVector((0 until lambda).map {
      i => if (fvals(i) > threFval) threFval + norm(pop.Z(::, i)) else fvals(i)
    }.toArray)

    val argFvals = argsort(scaledFvals)

    def sortWithFval(m: DenseMatrix[Double]): DenseMatrix[Double] =
      DenseMatrix((0 until lambda).map(i => m(::, argFvals(i)).toArray): _*).t

    (Population(sortWithFval(pop.Z), sortWithFval(pop.Y), sortWithFval(pop.X)),
      DenseVector(scaledFvals.toArray.sorted).map(v => if (v > threFval) Double.PositiveInfinity else v)
    )
  }

  def update(pop: Population, fvals: DenseVector[Double]): FMNES = {

    val lambdaF = fvals.toArray.count(_ < Double.MaxValue)

    val wRankZMatrix = pop.Z * diag(weightsRank)
    val wRankZ = sum(wRankZMatrix(*, ::))

    val psN: DenseVector[Double] = (1.0 - csig) * ps + sqrt(csig * (2.0 - csig) * mueff) * wRankZ

    val weightsDist: DenseVector[Double] = {
      val wRankDistHat = (0 until lambda).map(i => weightsRankHat(i) * wDistHat(pop.Z(::, i), lambdaF))
      val wSum = sum(wRankDistHat)
      DenseVector(wRankDistHat.map(w => w / wSum - 1.0 / lambda).toArray)
    }

    val weights = if (norm(psN) >= chiN) weightsDist else weightsRank

    val etaSigma = if (norm(psN) >= chiN) getEtaSigmaMove
    else if (norm(psN) >= 0.1 * chiN) getEtaSigmaStag(lambdaF)
    else getEtaSigmaConv(lambdaF)

    val wzMatrix = pop.Z * diag(weights)
    val Gdelta = sum(wzMatrix(*, ::))

    val GM = (0 until lambda).map(i => (pop.Z(::, i) * pop.Z(::, i).t - DenseMatrix.eye[Double](dim)) *:* weights(i))
      .foldLeft(DenseMatrix.zeros[Double](dim, dim))(_ + _)

    val Gsigma = trace(GM) / dim.toDouble

    val preGB = GM - Gsigma * DenseMatrix.eye[Double](dim)
    val GB = (preGB + preGB.t) / 2.0

    val meanN: DenseVector[Double] = mean + etaM * sigma * B * Gdelta

    val pcN = (1.0 - cc) * pc + sqrt(cc * (2.0 - cc) * mueff) * B * Gdelta

    val lsig = if (Gsigma < 0.0 && norm(psN) >= chiN) 1.0 else 0.0
    val sigmaN = sigma * exp((1 - lsig) * etaSigma * Gsigma / 2.0)

    // eiadx-nes
    val nA = B * expm(getCMu(lambdaF) * GB) * B.t
    val A = B * B.t
    val EigSym(dVec, uMat) = eigSym(A)
    val tauVec = (0 until dim).map(i => (uMat(::, i).t * nA * uMat(::, i)) / (uMat(::, i).t * A * uMat(::, i)) - 1.0)
    val tauFlag = (0 until dim).map(i => if (tauVec(i) > 0) 1.0 else 0.0)
    val tau = max(tauVec)
    val gammaN = max((1 - cGamma) * gamma + cGamma * sqrt(1 + dGamma * tau), 1.0)

    val Q = (gammaN - 1) * (0 until dim)
      .map(i => tauFlag(i) * uMat(::, i) * uMat(::, i).t)
      .foldLeft(DenseMatrix.zeros[Double](dim, dim))(_ + _) +
      DenseMatrix.eye[Double](dim)
    val stepsizeQ = math.pow(det(Q), 1.0 / dim)
    val sigmaNN = stepsizeQ * sigmaN
    val nnA = Q * nA * Q / (stepsizeQ * stepsizeQ)

    val lc = if (norm(psN) >= 1.0 * chiN) 1.0 else 0.0
    val nnnA = nnA + lc * getC1(lambdaF) * (pcN * pcN.t - B * B.t)
    val stepsizeA = math.pow(det(nnnA), 1.0 / dim)
    val nnnnAPre = nnnA / stepsizeA
    val nnnnA = (nnnnAPre + nnnnAPre.t) / 2.0
    val EigSym(dVecN, uMatN) = eigSym(nnnnA)
    val nD = dVec.map(sqrt(_))
    val nB = uMatN * diag(nD) * uMatN.t

    new FMNES(
      iteration + 1,
      lambda,
      dim,
      psN,
      pcN,
      nB,
      sigmaN,
      meanN,
      gammaN,
      hInv
    )
  }
}

object FMNES {

  def apply(lambda: Int, initialM: DenseVector[Double], initialSigma: Double): FMNES = {
    new FMNES(1, lambda, initialM.length,
      DenseVector.zeros[Double](initialM.length), // ps
      DenseVector.zeros[Double](initialM.length), // pc
      DenseMatrix.eye[Double](initialM.length), // B
      initialSigma,
      initialM,
      1.0,
      Newton.optimize(initialM.length)
    )
  }

}
