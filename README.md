# asap: The Scala Evolutionary Computation Library
![asap logo](https://github.com/nmasahiro/asap/blob/master/asap_logo.png)


asap is scala evolutionary computation library for optimization.

This repository contains implementations of the following algorithms.

* CMA-ES
* DX-NES
* FM-NES


[![CircleCI](https://circleci.com/gh/nmasahiro/asap/tree/master.svg?style=shield)](https://circleci.com/gh/nmasahiro/asap/tree/master)


## Getting Started

This project can be build with SBT 0.13.x.

```sbtshell
// https://mvnrepository.com/artifact/com.github.nmasahiro/asap
libraryDependencies += "com.github.nmasahiro" % "asap" % "0.0.11"
)
```

This is an example using CMA-ES.

```scala
import breeze.linalg.DenseVector
import com.github.nmasahiro.asap.algorithm.StrategyDriver
import com.github.nmasahiro.asap.algorithm.cmaes.{CMAES, CMAWeightActive}
import com.github.nmasahiro.asap.util.ParallelObjectiveFunction
import com.github.nmasahiro.asap.util._

object SampleMain extends App {

  val ktablet = ParallelObjectiveFunction ({
    case x: DenseVector[Double] =>
      val dim = x.length
      (for (i <- 0 until dim) yield {
        if (i < (dim / 4.0).toInt) x(i) * x(i) else math.pow(100 * x(i), 2.0)
      }).sum
  })

  val driver = StrategyDriver(ktablet)

  val dim = 40
  val lambda = 8
  val initialM = 3.0 * DenseVector.ones[Double](dim)
  val initialSigma = 2.0
  val cmaes = CMAES(lambda, initialM, initialSigma, CMAWeightActive())

  val successFval = 1e-12
  val finishEvalCnt = (5 * dim * 1e4).toInt

  val (evalCnt, bestX) = driver.optimize(
    cmaes,
    fvalBestReached(successFval) orElse
      evalCntReached(finishEvalCnt) orElse
      proceed
  )

  println(s"evalCnt:$evalCnt, bestX:$bestX")

}
```


## Versioning

We use [SemVer](http://semver.org/) for versioning. For the versions available, see the [tags on this repository](https://github.com/nmasahiro/crfmnes/tags). 


## License

This project is licensed under the MIT License - see the [LICENSE](https://github.com/nmasahiro/asap/blob/master/LICENSE) file for details
