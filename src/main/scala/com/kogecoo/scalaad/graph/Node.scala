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

trait ContainerNode[U[_], T] extends Node[U, T] {

  type C = ContainerNode[U, T]
  type N = NonContainerNode[U, T]

  def apply(): U[T]
  def deriv(wrt: Node[U, T]): U[T] // compute with forward-mode automatic differentiation
  def propagate(g: U[T]): U[T]    // compute with reverse-mode autmatic differentiation
  def grad()(implicit vr: ValueRule[U, T]): U[T] = {
    propagate(vr.zeroMul(this())) // FIXME: perhaps getting shape reference by apply() is expensive
  }

  def +(rhs: N)(implicit vr: ValueRule[U, T]): C = Add_CN(this, rhs)
  def -(rhs: N)(implicit vr: ValueRule[U, T]): C = Sub_CN(this, rhs)
  def *(rhs: N)(implicit vr: ValueRule[U, T]): C = Mul_CN(this, rhs)
  def /(rhs: N)(implicit vr: ValueRule[U, T]): C = Div_CN(this, rhs)

  def +(rhs: C)(implicit vr: ValueRule[U, T]): C = Add_CC(this, rhs)
  def -(rhs: C)(implicit vr: ValueRule[U, T]): C = Sub_CC(this, rhs)
  def *(rhs: C)(implicit vr: ValueRule[U, T]): C = Mul_CC(this, rhs)
  def /(rhs: C)(implicit vr: ValueRule[U, T]): C = Div_CC(this, rhs)

  def unary_+()(implicit vr: ValueRule[U, T]): C = Pos_C(this)
  def unary_-()(implicit vr: ValueRule[U, T]): C = Neg_C(this)

}

trait NonContainerNode[U[_], T] extends Node[U, T] {

  type C = ContainerNode[U, T]
  type N = NonContainerNode[U, T]

  def apply(): T
  def deriv(wrt: Node[U, T]): T
  def propagate(g: T): T
  def grad()(implicit vr: ValueRule[U, T]): T = {
    propagate(vr.zeroMul)
  }

  def +(rhs: N)(implicit vr: ValueRule[U, T]): N = Add_NN(this, rhs)
  def -(rhs: N)(implicit vr: ValueRule[U, T]): N = Sub_NN(this, rhs)
  def *(rhs: N)(implicit vr: ValueRule[U, T]): N = Mul_NN(this, rhs)
  def /(rhs: N)(implicit vr: ValueRule[U, T]): N = Div_NN(this, rhs)

  def +(rhs: C)(implicit vr: ValueRule[U, T]): C = Add_NC(this, rhs)
  def -(rhs: C)(implicit vr: ValueRule[U, T]): C = Sub_NC(this, rhs)
  def *(rhs: C)(implicit vr: ValueRule[U, T]): C = Mul_NC(this, rhs)
  def /(rhs: C)(implicit vr: ValueRule[U, T]): C = Div_NC(this, rhs)

  def unary_+()(implicit vr: ValueRule[U, T]): N = Pos_N(this)
  def unary_-()(implicit vr: ValueRule[U, T]): N = Neg_N(this)

}

