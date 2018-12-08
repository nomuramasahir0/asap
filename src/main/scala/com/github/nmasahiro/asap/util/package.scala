package com.github.nmasahiro.asap

import breeze.linalg.{*, DenseMatrix, DenseVector}

package object util {

  type StopCondition = PartialFunction[(Int, Double), Boolean]

  val proceed: StopCondition = {
    case (itr: Int, fvalBest: Double) => false
  }

  def fvalBestReached(fvalMin: Double): StopCondition = {
    case (_, fvalBest: Double) => if fvalBest < fvalMin => true
  }

  case class SimpleObjectiveFunction(objFunction: PartialFunction[DenseVector[Double], Double]) extends PartialFunction[DenseMatrix[Double], DenseVector[Double]] {
    def apply(pop: DenseMatrix[Double]): DenseVector[Double] = {
      pop(*, ::).map {
        x => objFunction.apply(x)
      }
    }
    def isDefinedAt(pop: DenseMatrix[Double]): Boolean = {
      pop(*, ::).map { x =>
        objFunction.isDefinedAt(x)
      }.reduce((a, b) => a && b)
    }
  }

  case class ParallelObjectiveFunction(objFunction: PartialFunction[DenseVector[Double], Double]) extends PartialFunction[DenseMatrix[Double], DenseVector[Double]] {
    def apply(pop: DenseMatrix[Double]): DenseVector[Double] = {
      DenseVector {
        pop(*, ::).iterator.toSeq.par.map {
          x => objFunction.apply(x)
        }.toArray
      }
    }
    def isDefinedAt(pop: DenseMatrix[Double]): Boolean = {
      pop(*, ::).iterator.toSeq.par.map(objFunction.isDefinedAt).reduce {
        (a, b) => a && b
      }
    }
  }

}
