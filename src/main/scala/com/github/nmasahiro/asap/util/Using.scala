package com.github.nmasahiro.util

import java.io.PrintWriter

object Using {

  def apply[A, B](resource: A)(process: A => B)(implicit closer: Closer[A]): B =
    try {
      process(resource)
    } finally {
      closer.close(resource)
    }

}

case class Closer[-A](close: A => Unit)

object Closer {
  implicit val pwCloser: Closer[PrintWriter] = Closer(_.close())
}
