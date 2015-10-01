package com.kogecoo.scalaad.graph

import com.kogecoo.scalaad.rule.ValueRule

import scala.language.higherKinds


// The most fundamental component of computational graph for automatic derivation.
// Every node in a computational graph must inherit Node.
//
//  Node doesn't know actual computational rule (how to calculate +, -, *, ...),
// so if you want to compute derivation on your own class (ComplexNumber, Matrix, etc),
// you need to define its computational rules (see the definition of ValueRule).

trait Node[U[_], T] {
  def toString: String
}

trait ContainerNode[U[_], T] {
  type C = ContainerNode[U, T]
  type N = NonContainerNode[U, T]

  implicit val vr: ValueRule[U, T]

  def apply(): U[T]
  def deriv(wrt: Node[U, T]): U[T] // compute with forward-mode automatic differentiation
  def propagate(g: U[T]): U[T]    // compute with reverse-mode autmatic differentiation
  def grad()(implicit vr: ValueRule[U, T]): U[T] = {
    propagate(vr.zeroMul)
  }

  def +(rhs: N): C = Add_CN(this, rhs)
  def -(rhs: N): C = Sub_CN(this, rhs)
  def *(rhs: N): C = Mul_CN(this, rhs)
  def /(rhs: N): C = Div_CN(this, rhs)

  def +(rhs: C): C = Add_CC(this, rhs)
  def -(rhs: C): C = Sub_CC(this, rhs)
  def *(rhs: C): C = Mul_CC(this, rhs)
  def /(rhs: C): C = Div_CC(this, rhs)

  def unary_+(): C = Pos_C(this)
  def unary_-(): C = Neg_C(this)

}

trait NonContainerNode[U[_], T] {

  type C = ContainerNode[U, T]
  type N = NonContainerNode[U, T]

  implicit val vr: ValueRule[U, T]

  def apply(): T
  def deriv(wrt: Node[U, T]): T
  def propagate(g: T): T
  def grad()(implicit vr: ValueRule[U, T]): T = {
    propagate(vr.zeroMul)
  }

  def +(rhs: N): N = Add_NN(this, rhs)
  def -(rhs: N): N = Sub_NN(this, rhs)
  def *(rhs: N): N = Mul_NN(this, rhs)
  def /(rhs: N): N = Div_NN(this, rhs)

  def +(rhs: C): C = Add_NC(this, rhs)
  def -(rhs: C): C = Sub_NC(this, rhs)
  def *(rhs: C): C = Mul_NC(this, rhs)
  def /(rhs: C): C = Div_NC(this, rhs)

  def unary_+(): N = Pos_N(this)
  def unary_-(): N = Neg_N(this)

}

