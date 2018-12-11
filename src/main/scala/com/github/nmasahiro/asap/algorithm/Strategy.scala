package com.github.nmasahiro.asap.algorithm

import breeze.linalg.{DenseMatrix, DenseVector}

// dim x lambda
case class Population(Z: DenseMatrix[Double], Y: DenseMatrix[Double], X: DenseMatrix[Double])

trait Strategy {

  def getLambda: Int

  def sampling: Population

  def sorted(pop: Population, fvals: DenseVector[Double]): (Population, DenseVector[Double])

  def update(pop: Population, fvals: DenseVector[Double]): Strategy

}

