package com.kogecoo.scalaad.graph

import com.kogecoo.scalaad.rule.Implicits._
import com.kogecoo.scalaad.rule.{ValueRule, ValueWrapperRule}

import scala.language.higherKinds


class ContainerVar[U[_], T](data: U[T])(implicit vr: ValueRule[U, T]) extends ContainerNode[U, T] {
  var gradient: U[T] = vr.zeroAdd(data)

  override def toString: String = s"Var[${ data }]"
  override def apply(): U[T] = data
  override def deriv(wrt: Node[U, T]): U[T] = {
    if (wrt == this) {
      vr.zeroMul(data)
    } else {
      vr.zeroAdd(data)
    }
  }

  override def propagate(g: U[T]): U[T] = { gradient += g * vr.zeroMul; g }
}

class NonContainerVar[U[_], T](data: T)(implicit vr: ValueRule[U, T]) extends NonContainerNode[U, T] {
  var gradient: T = vr.zeroAdd

  override def toString: String = s"Var[${ data }]"
  override def apply(): T = data
  override def deriv(wrt: Node[U, T]): T = {
    if (wrt == this) {
      vr.zeroMul
    } else {
      vr.zeroAdd
    }
  }

  override def propagate(g: T): T = { gradient += g * vr.zeroMul; g }
}

object Var {
  def apply[U[_], T](data: U[T])(implicit r: ValueRule[U, T]): ContainerVar[U, T] = new ContainerVar[U, T](data)
  def apply[U[_], T](data: T)(implicit r: ValueRule[U, T]): NonContainerVar[U, T] = new NonContainerVar[U, T](data)
  /*def apply[Src, U[_], T](data: Src)(implicit r: ValueRule[U, T], f: ValueWrapperRule[Src, U, T]): Var[U, T] = {
    new Var[U, T](f.toWrapper(data))
  }*/
}
