package com.kogecoo.scalaad.graph

import com.kogecoo.scalaad.rule.Implicits._
import com.kogecoo.scalaad.rule.ValueRule

import scala.language.higherKinds


case class ScalarConst[U[_], T](data: T)(implicit vr: ValueRule[U, T]) extends NonContainerNode[U, T] {
  override def toString: String = data.toString
  override def apply(): T = data
  override def deriv(wrt: Node[U, T]): T = vr.zeroAdd
  override def propagate(g: T): T = g * vr.zeroAdd
}

case class ContainerConst[U[_], T](data: U[T])(implicit vr: ValueRule[U, T]) extends ContainerNode[U, T] {
  override def toString: String = data.toString
  override def apply(): U[T] = data
  override def deriv(wrt: Node[U, T]): U[T] = vr.zeroAdd(data)
  override def propagate(g: U[T]): U[T] = g * vr.zeroAdd(data)
}

