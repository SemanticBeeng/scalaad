package com.kogecoo.scalaad.graph

import com.kogecoo.scalaad.rule.Implicits._
import com.kogecoo.scalaad.rule.{ValueRule, ValueWrapperRule}

import scala.language.higherKinds


class Var[U[_], T](data: U[T])(implicit override val vr: ValueRule[U, T]) extends ContainerNode[U, T] {
  var gradient: U[T] = vr.zeroAdd

  override def toString: String = s"Var[${ data }]"
  override def apply(): U[T] = data
  override def deriv(wrt: Node[U, T]): U[T] = {
    if (wrt == this) {
      vr.zeroMul
    } else {
      vr.derivConst
    }
  }

  override def propagate(g: U[T]): U[T] = { gradient += g * vr.zeroMul; g }
}

object Var {
  def apply[U[_], T](data: U[T])(implicit r: ValueRule[U, T]): Var[U, T] = new Var[U, T](data)
  def apply[Src, U[_], T](data: Src)(implicit r: ValueRule[U, T], f: ValueWrapperRule[Src, U, T]): Var[U, T] = {
    new Var[U, T](f.toWrapper(data))
  }
}
