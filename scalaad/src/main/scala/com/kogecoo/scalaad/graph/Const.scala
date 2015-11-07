package com.kogecoo.scalaad.graph

import com.kogecoo.scalaad.rule.ValueRule
import com.kogecoo.scalaad.value.Value

import shapeless.Nat
import Predef.{ any2stringadd => _, _ }
import scala.language.higherKinds
/*
abstract class ConstBase[U[_], T, Rank <: Nat](implicit r: ValueRule[U, T]) extends Node[U, T, Rank]

// TODO: use Coproducts and discriminated unions ? e.g. (data: T :+: U[T])
case class Const[U[_], T, Rank <: Nat](data: U[T])(implicit r: ValueRule[U, T]) extends ConstBase[U, T, Rank] {
  override def toString: String = data.toString
  override def apply(): Value[U, T] = r.toValue(data)
  override def deriv[K <: Nat](wrt: Var[U, T, K]): N = Zero[U, T, Rank](data)
  override def propagate[R <: Nat, OutRank <: Nat](g: Node[U, T, R]): Node[U, T, OutRank] = {
    Mul(g, Zero[U, T, Rank](data))
  }
}

case class One[U[_], T, Rank <: Nat](data: U[T])(implicit r: ValueRule[U, T]) extends ConstBase[U, T, Rank] {
  override def toString: String = "One"
  override def apply(): Value[U, T] = r.one
  override def deriv[R <: Nat](wrt: Var[U, T, R]): N =  Zero[U, T, Rank](data)
  override def propagate[R <: Nat, OutRank <: Nat](g: Node[U, T, R]): Node[U, T, OutRank] = {
    Mul(g, Zero[U, T, Rank](data))
  }
}

case class Zero[U[_], T, Rank <: Nat](data: U[T])(implicit r: ValueRule[U, T]) extends ConstBase[U, T, Rank] {
  override def toString: String = "Zero"
  override def apply(): Value[U, T] = r.one
  override def deriv[R <: Nat](wrt: Var[U, T, R]): N = Zero[U, T, Rank](data)
  override def propagate[K <: Nat](g: NK[K]): Node[U, T, Rank] = {
    g + Zero[U, T, Rank](data)
  }
}
*/
