package scalaad.impl.breeze.op

import breeze.linalg.DenseVector

import scalaad.graph.Real
import scalaad.impl.breeze.{T0, T1, T2, T3, T4}


trait BroadcastOp extends BroadcastOpBase[Real, T0, T1, T2, T3, T4, T0, T1, T2, T3, T4] {

  def higher1(a: T0): T1 = DenseVector(a)

  def higher2(a: T1): T2 = a.toDenseMatrix

  def higher3(a: T2): T3 = DenseVector(a)

  def higher4(a: T3): T4 = a.toDenseMatrix


  def broadcast111(a: T1, b: T1): T1 =
    BroadcastHelper.perform_T1_T1_T1(a, b, baseOpImpl101, baseOpImpl111, exchangable)

  def broadcast222(a: T2, b: T2): T2 =
    BroadcastHelper.perform_2_2_2[T0, T0](a, b, baseOpImpl202, baseOpImpl222, exchangable)

  def broadcast333(a: T3, b: T3): T3 =
    BroadcastHelper.perform_3_3_3[T0, T0](a, b, baseOpImpl202, baseOpImpl222, exchangable)

}
