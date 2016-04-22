package com.kogecoo.scalaad.node

import com.kogecoo.scalaad.Shape


/**
  * represents applying Binary condition, which takes 2 BoolNodes as arguments.
 *
  * @tparam S a shape of this BoolNode
  * @tparam L a shape of left BoolNode
  * @tparam R a shape of right BoolNode
  */
trait BinaryCondition[S <: Shape, L <: Shape, R <: Shape] extends BooleanTerm[S] {
  val shape: S
  def l: BooleanTerm[L]
  def r: BooleanTerm[R]
}

/**
  * specialized BinaryCondition, its left and output BoolNode are the same type of shape.
 *
  * @tparam L a shape of left and output BoolNode
  * @tparam R a shape of right BoolNode
  */
trait LeftShapedBinaryCondition[L <: Shape, R <: Shape] extends BinaryCondition[L, L, R] {
  override val shape: L = l.shape
  override def l: BooleanTerm[L]
  override def r: BooleanTerm[R]
}

/**
  * specific BinaryCondition its output BoolNode and right BoolNode have the same order and shape
 *
  * @tparam L a shape of left BoolNode
  * @tparam R a shape of right and output BoolNode
  */
trait RightShapedBinaryCondition[L <: Shape, R <: Shape] extends BinaryCondition[R, L, R] {
  override val shape: R = r.shape
  override def l: BooleanTerm[L]
  override def r: BooleanTerm[R]
}

/**
  * specialized BinaryNode, its left, right and output BoolNode are the same type of shape.
 *
  * @tparam S type of shape for left, right and output BoolNode
  */
trait SameShapedBinaryCondition[S <: Shape] extends BinaryCondition[S, S, S] {
  override val shape: S = l.shape
  override def l: BooleanTerm[S]
  override def r: BooleanTerm[S]
}

/**
  * represents applying unary condition, which takes 1 BoolNode as argument.
 *
  * @tparam O type of shape for output BoolNode
  * @tparam S type of shape for argument BoolNode
  */
trait UnaryCondition[O <: Shape, S <: Shape] extends BooleanTerm[O] {
  val shape: O

  def v: BooleanTerm[S]
}

/**
  * specialized UnaryCondition, its input and output BoolNode are the same type of shape.
 *
  * @tparam S type of shape for left, right and output BoolNode
  */
trait SameShapedUnaryCondition[S <: Shape] extends UnaryCondition[S, S] {
  override val shape: S = v.shape
  override def v: BooleanTerm[S]
}

