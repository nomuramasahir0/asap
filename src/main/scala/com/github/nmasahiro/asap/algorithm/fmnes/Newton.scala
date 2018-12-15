/*
 * Copyright (c) 2018 Masahiro Nomura
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy of
 * this software and associated documentation files (the "Software"), to deal in
 * the Software without restriction, including without limitation the rights to
 * use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of
 * the Software, and to permit persons to whom the Software is furnished to do so,
 * subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all
 * copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS
 * FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR
 * COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
 * IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
 * CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 */

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
