package com.kogecoo.scalaad.node

import com.kogecoo.scalaad.op.{Op0, Op00}
import com.kogecoo.scalaad.{Shape, Shape0, Shape1, Shape2}

import scala.language.higherKinds

/**
  * represents applying binary operation, which takes 2 Nodes as arguments.
 *
  * @tparam S a shape of this Node
  * @tparam L a shape of left Node
  * @tparam R a shape of right Node
  */
trait Application2[T[_] <: Term[S], S <: Shape, L <: Shape, R <: Shape] extends ValueTerm[S] {
  val shape: S
  def l: ValueTerm[L]
  def r: ValueTerm[R]
}

/**
  * specialized BinaryNode, its left and output Node are the same type of shape.
 *
  * @tparam L a shape of left and output Node
  * @tparam R a shape of right Node
  */
trait LeftShapedApplication2[T[_] <: Term[L], L <: Shape, R <: Shape] extends Application2[T, L, L, R] {
  override val shape: L = l.shape
  override def l: T[L]
  override def r: T[R]
}

/**
  * specialized BinaryNode, its right and output Node are the same type of shape.
 *
  * @tparam L a shape of left Node
  * @tparam R a shape of right and output Node
  */
trait RightShapedBinaryValueTerm[L <: Shape, R <: Shape] extends Application2[R, L, R] {
  override val shape: R = r.shape
  override def l: ValueTerm[L]
  override def r: ValueTerm[R]
}

/**
  * specialized BinaryNode, its left, right and output Node are the same type of shape.
 *
  * @tparam S type of shape for left, right and output Node
  */
trait SameShapedBinaryValueTerm[S <: Shape] extends Application2[S, S, S] {
  override val shape: S = l.shape
  override def l: ValueTerm[S]
  override def r: ValueTerm[S]
}

/**
  * represents applying unary operation, which takes 1 Node as argument.
 *
  * @tparam O type of shape for output Node
  * @tparam S type of shape for argument Node
  */
trait UnaryValueTerm[O <: Shape, S <: Shape] extends ValueTerm[O] {
  val shape: O

  def v: ValueTerm[S]
}

/**
  * specialized UnaryNode, its input and output Node are the same type of shape.
 *
  * @tparam S type of shape for left, right and output Node
  */
trait SameShapedUnaryValueTerm[S <: Shape] extends UnaryValueTerm[S, S] {
  override val shape: S = v.shape

  override def v: ValueTerm[S]
}


// These nodes express higher-order functions

// UnaryNode

case class Apply0(v: ValueTerm[S0], op: Op0) extends SameShapedUnaryValueTerm[S0]

case class Apply00(l: ValueTerm[S0], r: ValueTerm[S0], op: Op00) extends SameShapedBinaryValueTerm[S0]


case class Elementwise1(v: ValueTerm[S1], op: Op0) extends SameShapedUnaryValueTerm[S1]

case class Elementwise2(v: ValueTerm[S2], op: Op0) extends SameShapedUnaryValueTerm[S2]


case class Broadcast1(v: ValueTerm[S1], op: Op0) extends SameShapedUnaryValueTerm[S1]

case class Broadcast2(v: ValueTerm[S2], op: Op0) extends SameShapedUnaryValueTerm[S2]


case class Fold1(v: ValueTerm[S1], op: Op00) extends UnaryValueTerm[S0, S1] { val shape: S0 = Shape0() }

case class Fold2(v: ValueTerm[S2], op: Op00) extends UnaryValueTerm[S0, S2] { val shape: S0 = Shape0() }


case class FoldRowwise2   (v: ValueTerm[S2], op: Op00) extends UnaryValueTerm[S1, S2] { val shape: S1 = Shape1(v.shape._1) }

case class FoldColumnwise2(v: ValueTerm[S2], op: Op00) extends UnaryValueTerm[S1, S2] { val shape: S1 = Shape1(v.shape._2) }


// Experimental UnaryNode

case class VecFill(v: N0, shape: S1) extends UnaryValueTerm[S1, S0]

case class MatFill(v: N0, shape: S2) extends UnaryValueTerm[S2, S0]


case class MatFillAcrossRow(v: N1, numColumns: Int) extends UnaryValueTerm[S2, S1] { val shape: S2 = Shape2(v.shape._1, numColumns) }

case class MatFillAcrossColumn(v: N1, numRows: Int) extends UnaryValueTerm[S2, S1] { val shape: S2 = Shape2(numRows, v.shape._1) }


// BinaryNode

case class Elementwise11(l: N1, r: N1, op: Op00) extends SameShapedBinaryValueTerm[S1]

case class Elementwise22(l: N2, r: N2, op: Op00) extends SameShapedBinaryValueTerm[S2]


case class Broadcast01(l: N0, r: N1, op: Op00) extends RightShapedBinaryValueTerm[S0, S1]

case class Broadcast02(l: N0, r: N2, op: Op00) extends RightShapedBinaryValueTerm[S0, S2]

case class Broadcast10(l: N1, r: N0, op: Op00) extends LeftShapedBinaryCondition[S1, S0]

case class Broadcast20(l: N2, r: N0, op: Op00) extends LeftShapedBinaryCondition[S2, S0]


case class Rowwise12(l: N1, r: N2, op: Op00) extends RightShapedBinaryValueTerm[S1, S2]

case class Rowwise21(l: N2, r: N1, op: Op00) extends LeftShapedBinaryCondition[S2, S1]


case class Columnwise12(l: N1, r: N2, op: Op00) extends RightShapedBinaryValueTerm[S1, S2]

case class Columnwise21(l: N2, r: N1, op: Op00) extends RightShapedBinaryValueTerm[S2, S1]


// TODO
case object Transpose1

case object Transpose2

