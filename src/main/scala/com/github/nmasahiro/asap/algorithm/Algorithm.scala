package com.github.nmasahiro.asap.algorithm

import breeze.linalg.{DenseMatrix, DenseVector}

abstract class Algorithm[AlgorithmState] {

  import cats.data.{State => CState}
  import CState.inspect

  // これはコンストラクタ的な役割
  // constructor with transparency (かっこよく言うと)
  def initialState(dim: Int, lambda: Int, initialM: DenseVector[Double], sigma: Double): AlgorithmState
  def sampling(state: AlgorithmState): DenseMatrix[Double]
  // これはStateを(参照，更新含めStateを必要としないので，)インターフェースを準備する必要ない．algorithmのinstance側で実装するだけでok
  def sort(pop: DenseMatrix[Double], fvals: DenseVector[Double]): DenseMatrix[Double]
  def updateState(state: AlgorithmState, sortedPop: DenseMatrix[Double]): AlgorithmState
  def incrementStep(state: AlgorithmState): AlgorithmState

  def sampling: CState[AlgorithmState, DenseMatrix[Double]] = inspect {
    state => sampling(state)
  }

  def updateState(sortedPop: DenseMatrix[Double]): CState[AlgorithmState, AlgorithmState] = inspect {
    state => updateState(state, sortedPop)
  }

  def incrementStep: CState[AlgorithmState, AlgorithmState] = inspect { state => incrementStep(state) }

}
