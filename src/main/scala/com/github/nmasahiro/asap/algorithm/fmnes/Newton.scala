package com.github.nmasahiro.asap.algorithm.fmnes

import breeze.numerics.exp

import scala.annotation.tailrec

object Newton {

  def f(a: Double, dim: Int): Double = {
    ((1 + a * a) * exp(a * a / 2.0) / 0.24) - 10.0 - dim
  }

  def fprime(a: Double): Double = {
    (1.0 / 0.24) * a * exp(a * a * 0.5) * (3.0 + a * a)
  }

  def optimize(dim: Int): Double = {
    @tailrec
    def optimize(hInv: Double): Double = {
      if (math.abs(f(hInv, dim)) < 1e-10) {
        hInv
      } else {
        optimize(hInv - (f(hInv, dim) / fprime(hInv)))
      }
    }
    optimize(1.0)
  }

}
