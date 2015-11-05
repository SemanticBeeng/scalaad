package com.kogecoo.scalaad.graph

import com.kogecoo.scalaad.rule.ValueRule
import com.kogecoo.scalaad.value.{ContainerValue, NonContainerValue, Value}

import shapeless.Nat

import scala.language.higherKinds


case class Var0[T](data: T) extends N0[T] {
  val r = implicitly[ValueRule[U, T]]

  override def toString: String = s"ScalarVar(${ data })"

  override def apply(): T = data

  override def deriv0      (wrt: N0[T]   ): N0[T] = if (wrt == this) One0 else Zero0
  override def deriv1[V[_]](wrt: N1[V, T]): N1[V, T] = Zero1
  override def deriv2[M[_]](wrt: N2[M, T]): N2[M, T] = Zero2

  override def propagate0      (g: N0[T]   ): N0[T]    = g * One0
  override def propagate1[V[_]](g: N1[V, T]): N1[V, T] = g * One1
  override def propagate2[M[_]](g: N2[M, T]): N2[M, T] = g * One2

}


class Var1[V[_], T](data: V[T]) extends N1[V, T] {
  val r = implicitly[ValueRule[U, T]]

  override def toString: String = s"VectorVar(${ data })"

  override def toString: String = data.toString

  override def apply(): V[T] = data

  def deriv0      (wrt: N0[T]   ): N1[V, T] = Zero1
  def deriv1[M[_]](wrt: N1[V, T]): N2[M, T] = if (wrt == this) One2 else Zero2

  def propagate0(g: N0[T]): N1[V, T]          = g * One1
  def propagate1[M[_]](g: N1[V, T]): N2[M, T] = g * One2

}


class Var2[M[_], T](data: M[T]) extends N2[M, T] {
  val r = implicitly[ValueRule[U, T]]

  override def toString: String = s"MatrixVar(${ data })"

  override def toString: String = data.toString

  def apply(): M[T] = data

  def deriv0(wrt: N0[T]): N2[M, T] = if (wrt == this) One2 else Zero2

  def propagate0(g: N0[T]): N2[M, T] = g * One2

}

object Var {
  def apply[T](v: T)(implicit r: ValueRule[U, T]): Var0 = Var0[T](v)

}
