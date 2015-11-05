package com.kogecoo.scalaad.graph

import com.kogecoo.scalaad.rule.ValueRule

import scala.language.higherKinds


class Const0[U[_], T](data: T) extends N0[T] {

  override def toString: String = data.toString

  override def apply(): T = data

  override def deriv0      (wrt: N0[T]   ): N0[T] = ScalarZero
  override def deriv1[V[_]](wrt: N1[V, T]): N1[V, T] = VectorZero
  override def deriv2[M[_]](wrt: N2[M, T]): N2[M, T] = MatrixZero

  override def propagate0      (g: N0[T]   ): N0[T]    = g * ScalarZero
  override def propagate1[V[_]](g: N1[V, T]): N1[V, T] = g * VectorZero
  override def propagate2[M[_]](g: N2[M, T]): N2[M, T] = g * MatrixZero

}


class Const1[V[_], T](data: V[T]) extends N1[V, T] {

  override def toString: String = data.toString

  override def apply(): V[T] = data

  def deriv0      (wrt: N0[T]   ): N1[V, T] = VectorZero
  def deriv1[M[_]](wrt: N1[V, T]): N2[M, T] = MatrixZero

  def propagate0(g: N0[T]): N1[V, T]          = VectorZero
  def propagate1[M[_]](g: N1[V, T]): N2[M, T] = MatrixZero

}


class Const2[M[_], T](data: M[T]) extends N2[M, T] {

  override def toString: String = data.toString

  def apply(): M[T] = data

  def deriv0(wrt: N0[T]): N2[M, T] = MatrixZero

  def propagate0(g: N0[T]): N2[M, T] = MatrixZero

}

case class One0[U[_], T](implicit r: ValueRule[U, T]) extends Const0[U, T](r.one) {
  override def toString: String = "One"
}

case class One1[U[_], T](implicit r: ValueRule[U, T]) extends Const1[U, T](r.one) {
  override def toString: String = "One"
}

case class One2[U[_], T](implicit r: ValueRule[U, T]) extends Const2[U, T](r.one) {
  override def toString: String = "One"
}

case class Zero0[U[_], T](implicit r: ValueRule[U, T]) extends Const0[U, T](r.zero) {
  override def toString: String = "Zero"
}

case class Zero1[U[_], T](implicit r: ValueRule[U, T]) extends Const1[U, T](r.zero) {
  override def toString: String = "Zero"
}

case class Zero2[U[_], T](implicit r: ValueRule[U, T]) extends Const2[U, T](r.zero) {
  override def toString: String = "Zero"
}

