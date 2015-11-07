package com.kogecoo.scalaad.graph

import scala.language.higherKinds


trait UnaryOp[T, Shape, N] extends Node[T, Shape] {
  def v: N
}

trait BinaryOp[T, Shape, L, R] extends Node[T, Shape] {
  def left: L
  def right: R
}
