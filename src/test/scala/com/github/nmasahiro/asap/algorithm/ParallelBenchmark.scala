package com.github.nmasahiro.asap.algorithm

import breeze.linalg.DenseVector
import com.github.nmasahiro.asap.util.ParallelObjectiveFunction


object ParallelBenchmark {

  val sphere = ParallelObjectiveFunction ({
    case x: DenseVector[Double] =>
      (for (i <- 0 until x.length) yield {
        x(i) * x(i)
      }).sum
  })

  val ellipsoid = ParallelObjectiveFunction ({
    case x: DenseVector[Double] =>
      val dim = x.length
      (for (i <- 0 until dim) yield {
        math.pow(math.pow(1000, i / (dim - 1)) * x(i), 2)
      }).sum
  })

  val ktablet = ParallelObjectiveFunction ({
    case x: DenseVector[Double] =>
      val dim = x.length
      (for (i <- 0 until dim) yield {
        if (i < (dim / 4.0).toInt) x(i) * x(i) else math.pow(100 * x(i), 2.0)
      }).sum
  })

  val rosenbrock = ParallelObjectiveFunction ({
    case x: DenseVector[Double] =>
      (for (i <- 0 until x.length - 1) yield {
        100 * (math.pow(x(i), 2) - x(i + 1)) * (math.pow(x(i), 2) - x(i + 1)) +
          math.pow(x(i) - 1.0, 2)
      }).sum
  })

  val constraintSphere = ParallelObjectiveFunction ({
    case x: DenseVector[Double] if x.exists(_ < 0.0) => Double.PositiveInfinity
    case x: DenseVector[Double] =>
      (for (i <- 0 until x.length) yield {
        x(i) * x(i)
      }).sum
  })

}
