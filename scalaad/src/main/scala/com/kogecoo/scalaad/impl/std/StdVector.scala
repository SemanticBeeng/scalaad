package com.kogecoo.scalaad.impl.std

import com.kogecoo.scalaad.{Tensor1, Shape1, StdVec}
import com.kogecoo.scalaad.graph.S1


case class StdVector(data: StdVec[Double], transposed: Boolean=false) extends Tensor1 {
  def toStdFloat: StdVec[Float] = data.map(_.toFloat)
  def toStdDouble: StdVec[Double] = data

  override def shape: S1 = Shape1(data.size)
}
