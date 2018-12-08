package com.github.nmasahiro.asap.algorithm

import breeze.linalg.{DenseMatrix, DenseVector, min}
import cats.data.{State => CState}
import com.github.nmasahiro.asap.util.StopCondition

import scala.collection.mutable.ArrayBuffer

object TracedAlgorithmDriver {

  final case class Trace(bestValueOfPop: ArrayBuffer[Double])

  final case class State[AlgorithmState](itr: Int, lambda: Int, evalCnt: Int, algState: AlgorithmState, trace: Trace)

}

case class TracedAlgorithmDriver[AlgorithmState](alg: Algorithm[AlgorithmState], objFunction: PartialFunction[DenseMatrix[Double], DenseVector[Double]]) {
  import CState.{get, modify}
  import TracedAlgorithmDriver._

  private def setAlgState(s: AlgorithmState): CState[State[AlgorithmState], Unit] = modify[State[AlgorithmState]] { _.copy(algState = s) }
  private def updateTrace(bestValueOfPop: Double): CState[State[AlgorithmState], Unit] = modify[State[AlgorithmState]] { s =>
    s.trace.bestValueOfPop += bestValueOfPop
    s.copy()
  }

  private val driveStep: CState[State[AlgorithmState], Unit] = for {
    state <- get[State[AlgorithmState]]
    pop = alg.sampling.runA(state.algState).value
    fvals = objFunction(pop)
    sortedPop = alg.sort(pop, fvals)
    newState = alg.updateState(sortedPop).runA(state.algState).value
    _ <- setAlgState(newState)
    _ <- updateTrace(min(fvals))
    _ <- alg.incrementStep.runA(newState)
  } yield ()

  // 現時点での評価回数(evalCnt)での判断はできるけど，fvalBestをどう渡すことにすればよいか？
  // 一つの案は，driveStepの返り値としてfvalBestとxBestを返すというもの
  // (xBest, fvalBest) <- driveStep
  // このfvalBestをdriverStepsに放り込むようにし，fvalBestをstopFunctionに適用して終了判定を行う．
  private def driverSteps(itr: Int, evalCnt: Int, lambda: Int, stopFunction: StopCondition): CState[State[AlgorithmState], Unit] = stopFunction(evalCnt, fvalBest) match {
    case true =>
  }

}
