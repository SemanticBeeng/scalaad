package com.kogecoo.scalaad.impl.breeze


import breeze.linalg.DenseVector
import com.kogecoo.scalaad.graph.S1
import com.kogecoo.scalaad.{Shape1, StdVec, Tensor1}


case class BreezeVector(data: DenseVector[Double], transposed: Boolean = false) extends Tensor1 {
  def toStdFloat: StdVec[Float] = toStdDouble.map(_.toFloat)
  def toStdDouble: StdVec[Double] = data.toArray.toSeq

  override def shape: S1 = Shape1(data.size)
}

