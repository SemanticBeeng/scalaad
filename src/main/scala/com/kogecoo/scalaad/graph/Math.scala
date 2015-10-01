package com.kogecoo.scalaad.graph

import com.kogecoo.scalaad.rule.{ContainerValue, MathRule, NonContainerValue, Value}

import scala.language.higherKinds
/*

case class sin_C[U[_], T](v: Node[U, T])(implicit vr: MathRule[U, T]) extends UnaryOp_C[U, T] {
  override def toString: String = s"sin(${ v })"
  override def apply(): U[T] = vr.sinS(v())
  override def deriv(wrt: Node[U, T]): U[T] = cos(v()) * v.deriv(wrt)
  override def propagate(g: U[T]): U[T] = g * cos(v())
}

case class sin_N[U[_], T](v: Node[U, T])(implicit vr: MathRule[U, T]) extends UnaryOp_N[U, T] {
  override def toString: String = s"sin(${ v })"
  override def apply(): T = sin(v())
  override def deriv(wrt: Node[U, T]): T = cos(v()) * v.deriv(wrt)
  override def propagate(g: T): T = g * cos(v())
}

case class cos_C[U[_], T](v: Node[U, T])(implicit vr: MathRule[U, T]) extends UnaryOp_C[U, T] {
  override def toString: String = s"cos(${ v })"
  override def apply(): U[T] = cos(v())
  override def deriv(wrt: Node[U, T]): U[T] = -sin(v()) * v.deriv(wrt)
  override def propagate(g: U[T]): U[T] = -sin(v()) * g
}

case class cos_N[U[_], T](v: Node[U, T])(implicit vr: MathRule[U, T]) extends UnaryOp_N[U, T] {
  override def toString: String = s"cos(${ v })"
  override def apply(): T = cos(v())
  override def deriv(wrt: Node[U, T]): T = -sin(v()) * v.deriv(wrt)
  override def propagate(g: T): T = -sin(v()) * g
}

case class tan_C[U[_], T](v: Node[U, T])(implicit vr: MathRule[U, T]) extends UnaryOp_C[U, T] {
  override def toString: String = s"tan(${ v })"
  override def apply(): U[T] = tan(v())
  override def deriv(wrt: Node[U, T]): U[T] = {
    val tan_v_val = tan(v())
    v.deriv(wrt) * (vr.zeroMul + tan_v_val * tan_v_val)
  }

  override def propagate(g: U[T]): U[T] = {
    val tan_v_val = tan(v())
    v.propagate(g * (vr.zeroMul + tan_v_val * tan_v_val))
  }
}

case class tan_N[U[_], T](v: Node[U, T])(implicit vr: MathRule[U, T]) extends UnaryOp_N[U, T] {
  override def toString: String = s"tan(${ v })"
  override def apply(): T = tan(v())
  override def deriv(wrt: Node[U, T]): T = {
    val tan_v_val = tan(v())
    v.deriv(wrt) * (vr.zeroMul + tan_v_val * tan_v_val)
  }

  override def propagate(g: T): T = {
    val tan_v_val = tan(v())
    v.propagate(g * (vr.zeroMul + tan_v_val * tan_v_val))
  }
}

case class ln[U[_], T](v: Node[U, T])(implicit r: MathRule[U, T]) extends UnaryOp_C[U, T] {
  override def toString: String = s"ln(${ v })"
  override def apply(): U[T] = ln(v())
  override def deriv(wrt: Node[U, T]): U[T] = v.deriv(wrt) / v()
  override def propagate(g: U[T]): U[T] = v.propagate(g / v())
}

case class ln_N[U[_], T](v: Node[U, T])(implicit r: MathRule[U, T]) extends UnaryOp_N[U, T] {
  override def toString: String = s"ln(${ v })"
  override def apply(): T = ln(v())
  override def deriv(wrt: Node[U, T]): T = v.deriv(wrt) / v()
  override def propagate(g: T): T = v.propagate(g / v())
}

case class exp_C[U[_], T](v: Node[U, T])(implicit r: MathRule[U, T]) extends UnaryOp_C[U, T] {
  override def toString: String = s"exp(${ v })"
  override def apply(): U[T] = exp(v())
  override def deriv(wrt: Node[U, T]): U[T] = v.deriv(wrt) * v()
  override def propagate(g: U[T]): U[T] = v.propagate(g * v())
}

case class exp_N[U[_], T](v: Node[U, T])(implicit r: MathRule[U, T]) extends UnaryOp_N[U, T] {
  override def toString: String = s"exp(${ v })"
  override def apply(): T = exp(v())
  override def deriv(wrt: Node[U, T]): T = v.deriv(wrt) * v()
  override def propagate(g: T): T = v.propagate(g * v())
}*/
