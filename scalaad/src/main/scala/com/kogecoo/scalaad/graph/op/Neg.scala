package com.kogecoo.scalaad.graph.op

import com.kogecoo.scalaad.graph._

import scala.language.higherKinds


case class Neg0[T](v: N0[T]) extends N0[T] {
  val r = implicitly[ValueRule[U, T]]

  override def toString: String = s"-(${ v })"

  override def apply(): T = r.neg0(v)

  override def deriv0      (wrt: N0[T]   ): N0[T]    = -v.deriv0(wrt)
  override def deriv1[V[_]](wrt: N1[V, T]): N1[V, T] = -v.deriv1(wrt)
  override def deriv2[M[_]](wrt: N2[M, T]): N2[M, T] = -v.deriv2(wrt)

  override def propagate0      (g: N0[T]   ): N0[T]    = v.propagate0(-g)
  override def propagate1[V[_]](g: N1[V, T]): N1[V, T] = v.propagate1(-g)
  override def propagate2[M[_]](g: N2[M, T]): N2[M, T] = v.propagate2(-g)
}


case class Neg1[V[_], T](v: N1[V, T]) extends N1[V, T] {
  val r = implicitly[ValueRule[V, T]]

  override def toString: String = s"-(${ v })"

  override def apply(): V[T] = r.neg1(v)

  override def deriv0      (wrt: N0[T]   ): N1[V, T] = -v.deriv0(wrt)
  override def deriv1[M[_]](wrt: N1[V, T]): N2[M, T] = -v.deriv1[M](wrt)

  override def propagate0(g: N0[T]): N1[V, T]          = v.propagate0(-g)
  override def propagate1[M[_]](g: N1[V, T]): N2[M, T] = v.propagate1(-g)
}

case class Neg2[M[_], T](v: N2[M, T]) extends N2[M, T] {
  val r = implicitly[ValueRule[M, T]]

  override def toString: String = s"-(${ v })"

  override def apply(): M[T] = r.neg2(v)

  def deriv0(wrt: N0[T]): N2[M, T] = -v.deriv0(wrt)

  def propagate0(g: N0[T]): N2[M, T] = v.propagate0(-g)
}

