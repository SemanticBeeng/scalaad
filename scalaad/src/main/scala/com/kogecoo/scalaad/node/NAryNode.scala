package com.kogecoo.scalaad.node

import com.kogecoo.scalaad.Shape

/**
  * represents applying binary operation, which takes 2 Nodes as arguments.
 *
  * @tparam S a shape of this Node
  * @tparam L a shape of left Node
  * @tparam R a shape of right Node
  */
trait BinaryValueTerm[S <: Shape, L <: Shape, R <: Shape] extends ValueTerm[S] {
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
trait LeftShapedBinaryValueTerm[L <: Shape, R <: Shape] extends BinaryValueTerm[L, L, R] {
  override val shape: L = l.shape
  override def l: ValueTerm[L]
  override def r: ValueTerm[R]
}

/**
  * specialized BinaryNode, its right and output Node are the same type of shape.
 *
  * @tparam L a shape of left Node
  * @tparam R a shape of right and output Node
  */
trait RightShapedBinaryValueTerm[L <: Shape, R <: Shape] extends BinaryValueTerm[R, L, R] {
  override val shape: R = r.shape
  override def l: ValueTerm[L]
  override def r: ValueTerm[R]
}

/**
  * specialized BinaryNode, its left, right and output Node are the same type of shape.
 *
  * @tparam S type of shape for left, right and output Node
  */
trait SameShapedBinaryValueTerm[S <: Shape] extends BinaryValueTerm[S, S, S] {
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


