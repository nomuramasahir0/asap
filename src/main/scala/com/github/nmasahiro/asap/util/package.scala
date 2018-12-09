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
