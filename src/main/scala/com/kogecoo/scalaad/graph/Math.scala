package com.kogecoo.scalaad.graph

import com.kogecoo.scalaad.rule.MathRule
import com.kogecoo.scalaad.rule.Implicits._

import scala.language.higherKinds


case class sin_C[U[_], T](n: ContainerNode[U, T])(implicit r: MathRule[U, T]) extends UnaryOp_C[U, T] {
  override def toString: String = s"sin(${ n })"
  override def apply(): U[T] = r.sinS(n())
  override def deriv(wrt: Node[U, T]): U[T] = r.cosS(n()) * n.deriv(wrt)
  override def propagate(g: U[T]): U[T] = g * r.cosS(n())
}

case class sin_N[U[_], T](n: NonContainerNode[U, T])(implicit r: MathRule[U, T]) extends UnaryOp_N[U, T] {
  override def toString: String = s"sin(${ n })"
  override def apply(): T = r.sinM(n())
  override def deriv(wrt: Node[U, T]): T = r.cosM(n()) * n.deriv(wrt)
  override def propagate(g: T): T = g * r.cosM(n())
}

object sin {
  def apply[U[_], T](n: ContainerNode[U, T])(implicit r: MathRule[U, T]): ContainerNode[U, T] = sin_C(n)
  def apply[U[_], T](n: NonContainerNode[U, T])(implicit r: MathRule[U, T]): NonContainerNode[U, T] = sin_N(n)
}

case class cos_C[U[_], T](n: ContainerNode[U, T])(implicit r: MathRule[U, T]) extends UnaryOp_C[U, T] {
  override def toString: String = s"cos(${ n })"
  override def apply(): U[T] = r.cosS(n())
  override def deriv(wrt: Node[U, T]): U[T] = -r.sinS(n()) * n.deriv(wrt)
  override def propagate(g: U[T]): U[T] = -r.sinS(n()) * g
}

case class cos_N[U[_], T](n: NonContainerNode[U, T])(implicit r: MathRule[U, T]) extends UnaryOp_N[U, T] {
  override def toString: String = s"cos(${ n })"
  override def apply(): T = r.cosM(n())
  override def deriv(wrt: Node[U, T]): T = -r.sinM(n()) * n.deriv(wrt)
  override def propagate(g: T): T = -r.sinM(n()) * g
}

object cos{
  def apply[U[_], T](n: ContainerNode[U, T])(implicit r: MathRule[U, T]): ContainerNode[U, T] = cos_C(n)
  def apply[U[_], T](n: NonContainerNode[U, T])(implicit r: MathRule[U, T]): NonContainerNode[U, T] = cos_N(n)
}

case class tan_C[U[_], T](n: ContainerNode[U, T])(implicit r: MathRule[U, T]) extends UnaryOp_C[U, T] {
  override def toString: String = s"tan(${ n })"
  override def apply(): U[T] = r.tanS(n())
  override def deriv(wrt: Node[U, T]): U[T] = {
    val tan_v_val = r.tanS(n())
    n.deriv(wrt) * (r.addMS(r.zeroMul, tan_v_val * tan_v_val))
  }

  override def propagate(g: U[T]): U[T] = {
    val tan_v_val = r.tanS(n())
    n.propagate(g * r.addMS(r.zeroMul, tan_v_val * tan_v_val))
  }
}

case class tan_N[U[_], T](n: NonContainerNode[U, T])(implicit r: MathRule[U, T]) extends UnaryOp_N[U, T] {
  override def toString: String = s"tan(${ n })"
  override def apply(): T = r.tanM(n())
  override def deriv(wrt: Node[U, T]): T = {
    val tan_v_val = r.tanM(n())
    n.deriv(wrt) * (r.zeroMul + tan_v_val * tan_v_val)
  }

  override def propagate(g: T): T = {
    val tan_v_val = r.tanM(n())
    n.propagate(g * (r.zeroMul + tan_v_val * tan_v_val))
  }
}

object tan {
  def apply[U[_], T](n: ContainerNode[U, T])(implicit r: MathRule[U, T]): ContainerNode[U, T] = tan_C(n)
  def apply[U[_], T](n: NonContainerNode[U, T])(implicit r: MathRule[U, T]): NonContainerNode[U, T] = tan_N(n)
}

case class ln_C[U[_], T](n: ContainerNode[U, T])(implicit r: MathRule[U, T]) extends UnaryOp_C[U, T] {
  override def toString: String = s"ln(${ n })"
  override def apply(): U[T] = r.lnS(n())
  override def deriv(wrt: Node[U, T]): U[T] = n.deriv(wrt) / n()
  override def propagate(g: U[T]): U[T] = n.propagate(g / n())
}

case class ln_N[U[_], T](n: NonContainerNode[U, T])(implicit r: MathRule[U, T]) extends UnaryOp_N[U, T] {
  override def toString: String = s"ln(${ n })"
  override def apply(): T = r.lnM(n())
  override def deriv(wrt: Node[U, T]): T = n.deriv(wrt) / n()
  override def propagate(g: T): T = n.propagate(g / n())
}

object ln {
  def apply[U[_], T](n: ContainerNode[U, T])(implicit r: MathRule[U, T]): ContainerNode[U, T] = ln_C(n)
  def apply[U[_], T](n: NonContainerNode[U, T])(implicit r: MathRule[U, T]): NonContainerNode[U, T] = ln_N(n)
}

case class exp_C[U[_], T](n: ContainerNode[U, T])(implicit r: MathRule[U, T]) extends UnaryOp_C[U, T] {
  override def toString: String = s"exp(${ n })"
  override def apply(): U[T] = r.expS(n())
  override def deriv(wrt: Node[U, T]): U[T] = n.deriv(wrt) * n()
  override def propagate(g: U[T]): U[T] = n.propagate(g * n())
}

case class exp_N[U[_], T](n: NonContainerNode[U, T])(implicit r: MathRule[U, T]) extends UnaryOp_N[U, T] {
  override def toString: String = s"exp(${ n })"
  override def apply(): T = r.expM(n())
  override def deriv(wrt: Node[U, T]): T = n.deriv(wrt) * n()
  override def propagate(g: T): T = n.propagate(g * n())
}

object exp {
  def apply[U[_], T](n: ContainerNode[U, T])(implicit r: MathRule[U, T]): ContainerNode[U, T] = exp_C(n)
  def apply[U[_], T](n: NonContainerNode[U, T])(implicit r: MathRule[U, T]): NonContainerNode[U, T] = exp_N(n)
}

