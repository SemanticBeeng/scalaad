package com.kogecoo.scalaad.graph

import com.kogecoo.scalaad.rule.ValueRule
import com.kogecoo.scalaad.value.{ContainerValue, NonContainerValue, Value}

import shapeless.Nat

import scala.language.higherKinds


case class Var[U[_], T, Rank <: Nat](data: U[T])(implicit r: ValueRule[U, T]) extends Node[U, T, Rank] {
  var gradient: Value[U, T] = r.zero(data)

  override def toString: String = s"Var[${ data }]"
  override def apply(): Value[U, T] = r.toValue(data)
  override def deriv[R <: Nat](wrt: Var[U, T, R]): N = {
    if (wrt == this) {
      One[U, T, Rank](data)
    } else {
      Zero[U, T, Rank](data)
    }
  }

  override def propagate[R <: Nat, OutRank <: Nat](g: Node[U, T, R]): Node[U, T, OutRank] = {
    val v = Mul[U, T, R, Rank, OutRank](g, One[U, T, Rank](data))
    //gradient = Add(v, gradient)
    v
  }

}
