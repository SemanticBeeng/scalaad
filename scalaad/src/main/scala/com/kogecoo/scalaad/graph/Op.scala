package com.kogecoo.scalaad.graph

import com.kogecoo.scalaad.rule.ValueRule
import com.kogecoo.scalaad.value.Value

import shapeless.Nat
import shapeless.ops.nat.LT.<

import scala.language.higherKinds

object Evaluator {
  def eval[U[_], T](tree: Add00[U, T]) = { }
}


case class Add00[U[_], T, L <: Nat : Eq0, R <: Nat : Eq0](lhs: Node[U, T, L], rhs: Node[U, T, R])(implicit vr: ValueRule[U, T]) extends Node[U, T, L] {
  override def toString: String = s"(${ lhs.toString } + ${ rhs.toString })"
  override def apply(): Value[U, T] = lhs() + rhs()
  override def deriv[K <: Nat](wrt: Var[U, T, K]): N = Add00(lhs.deriv(wrt), rhs.deriv(wrt))
  override def propagate0[K <: Nat : Eq0[K]](g: Node[U, T, K]): Unit = lhs.propagate0(g), rhs.propagate0(g))
  override def propagate1[K <: Nat : Eq1[K]](g: Node[U, T, K]): Unit = lhs.propagate1(g), rhs.propagate1(g))
  override def propagate2[K <: Nat : Eq2[K]](g: Node[U, T, K]): Unit = lhs.propagate2(g), rhs.propagate2(g))
}

abstract class AddBase[U, T, Rank] extends Node[U, T, Rank] {
  override def toString: String = s"(${ lhs.toString } + ${ rhs.toString })"
  override def apply(): Value[U, T] = lhs() + rhs()
}

case class Add00[U[_], T](lhs: Node[U, T, Nat._0], rhs: Node[U, T, Nat._0])(implicit vr: ValueRule[U, T]) extends Node[U, T, Nat._0] {
}

case class Add01[U[_], T](lhs: Node[U, T, Nat._0], rhs: Node[U, T, Nat._1])(implicit vr: ValueRule[U, T]) extends Node[U, T, Nat._1]
case class Add02[U[_], T](lhs: Node[U, T, Nat._0], rhs: Node[U, T, Nat._2])(implicit vr: ValueRule[U, T]) extends Node[U, T, Nat._2]
case class Add10[U[_], T](lhs: Node[U, T, Nat._1], rhs: Node[U, T, Nat._0])(implicit vr: ValueRule[U, T]) extends Node[U, T, Nat._1]
case class Add11[U[_], T](lhs: Node[U, T, Nat._1], rhs: Node[U, T, Nat._1])(implicit vr: ValueRule[U, T]) extends Node[U, T, Nat._1]
case class Add20[U[_], T](lhs: Node[U, T, Nat._2], rhs: Node[U, T, Nat._0])(implicit vr: ValueRule[U, T]) extends Node[U, T, Nat._2]
case class Add22[U[_], T](lhs: Node[U, T, Nat._2], rhs: Node[U, T, Nat._2])(implicit vr: ValueRule[U, T]) extends Node[U, T, Nat._2]

case class Add0N[U[_], T, N <: Nat : Gt2[N]](lhs: Node[U, T, Nat._0], rhs: Node[U, T, N])(implicit vr: ValueRule[U, T]) extends Node[U, T, N]
case class AddN0[U[_], T, N <: Nat : Gt2[N](lhs: Node[U, T, N], rhs: Node[U, T, Nat._0])(implicit vr: ValueRule[U, T]) extends Node[U, T, N]
case class AddNN[U[_], T, N <: Nat : Gt2[N]](lhs: Node[U, T, N], rhs: Node[U, T, N])(implicit vr: ValueRule[U, T]) extends Node[U, T, N]

//case class Add12[U[_], T](lhs: Node[U, T, L], rhs: Node[U, T, R])(implicit vr: ValueRule[U, T]) extends Node[U, T, L]
//case class Add21[U[_], T](lhs: Node[U, T, L], rhs: Node[U, T, R])(implicit vr: ValueRule[U, T]) extends Node[U, T, L]
//case class AddR1[U[_], T](lhs: Node[U, T, L], rhs: Node[U, T, R])(implicit vr: ValueRule[U, T]) extends Node[U, T, L]
//case class AddR2[U[_], T](lhs: Node[U, T, L], rhs: Node[U, T, R])(implicit vr: ValueRule[U, T]) extends Node[U, T, L]


case class Add0R[U[_], T, L <: Nat : Eq0, R <: Nat : Gt0](lhs: Node[U, T, L], rhs: Node[U, T, R])(implicit vr: ValueRule[U, T]) extends Node[U, T, R] {
  override def toString: String = s"(${ lhs.toString } + ${ rhs.toString })"
  override def apply(): Value[U, T] = lhs() + rhs()
  override def deriv[K <: Nat](wrt: Var[U, T, K]): N = Add0R(lhs.deriv(wrt), rhs.deriv(wrt))
  override def propagate0[K <: Nat : Eq0[K]](g: Node[U, T, K]): N = Add0R(lhs.propagate0(g), rhs.propagate0(g))
  override def propagate1[K <: Nat : Eq1[K]](g: Node[U, T, K]): N = Add0R(lhs.propagate1(g), rhs.propagate1(g))
}

case class AddL0[U[_], T, L <: Nat : Gt0 , R <: Nat : Eq0](lhs: Node[U, T, L], rhs: Node[U, T, R])(implicit vr: ValueRule[U, T]) extends Node[U, T, L] {
  override def toString: String = s"(${ lhs.toString } + ${ rhs.toString })"
  override def apply(): Value[U, T] = lhs() + rhs()
  override def deriv[K <: Nat](wrt: Var[U, T, K]): N = lhs.deriv(wrt) +: rhs.deriv(wrt)
  //override def deriv[K <: Nat](wrt: Var[U, T, K]): N = AddL0(lhs.deriv(wrt), rhs.deriv(wrt))
  override def propagate[K <: Nat](g: Node[U, T, K]): Node[U, T, K] = AddL0(lhs.propagate(g), rhs.propagate(g))
}

case class AddLR[U[_], T, L <: Nat : Gt0, R <: Nat : Gt0](lhs: Node[U, T, L], rhs: Node[U, T, R])(implicit vr: ValueRule[U, T], ev: L =:= R) extends Node[U, T, L] {
  override def toString: String = s"(${ lhs.toString } + ${ rhs.toString })"
  override def apply(): Value[U, T] = lhs() + rhs()
  override def deriv[K <: Nat](wrt: Var[U, T, K]): N = AddLR(lhs.deriv(wrt), rhs.deriv(wrt))
  override def propagate[K <: Nat](g: Node[U, T, K]): N = AddLR(lhs.propagate(g), rhs.propagate(g))
}

case class Sub[U[_], T, RankL <: Nat, RankR <: Nat, OutRank <: Nat](lhs: Node[U, T, RankL], rhs: Node[U, T, RankR])(implicit vr: ValueRule[U, T]) extends Node[U, T, OutRank] {
  override def toString: String = s"(${ lhs.toString } - ${ rhs.toString })"
  override def apply(): Value[U, T] = lhs() - rhs()
  override def deriv[K <: Nat](wrt: Var[U, T, K]): Node[U, T, OutRank] = Sub(lhs.deriv(wrt), rhs.deriv(wrt))
  override def propagate[K <: Nat](g: Node[U, T, K]): Node[U, T, OutRank] = Sub(lhs.propagate(g), rhs.propagate(g))
}

case class Mul[U[_], T, RankL <: Nat, RankR <: Nat, OutRank <: Nat](lhs: Node[U, T, RankL], rhs: Node[U, T, RankR])(implicit vr: ValueRule[U, T]) extends Node[U, T, OutRank] {
  override def toString: String = s"(${ lhs.toString } * ${ rhs.toString })"
  override def apply(): Value[U, T] = lhs() * rhs()
  override def deriv[R <: Nat, OutRank <: Nat](wrt: Var[U, T, R]): Node[U, T, OutRank] = {
    val ld = Mul(lhs.deriv(wrt), rhs)
    val rd = Mul(lhs, rhs.deriv(wrt))
    Add(ld, rd)
  }
  override def propagate[R <: Nat, OutRank <: Nat](g: Node[U, T, R]): Node[U, T, OutRank] = {
    val lg = Mul(g, rhs)
    val rg = Mul(lhs, g)
    Add(lg, rg)
  }
}

case class Div[U[_], T, RankL <: Nat, RankR <: Nat, OutRank <: Nat](lhs: Node[U, T, RankL], rhs: Node[U, T, RankR])(implicit vr: ValueRule[U, T]) extends Node[U, T, OutRank] {//BinaryOp[U, T, RankL, RankR, OutRank] {
  override def toString: String = s"(${ lhs.toString } / ${ rhs.toString })"
  override def apply(): Value[U, T] = lhs() / rhs()
  override def deriv[R <: Nat, OutRank <: Nat](wrt: Var[U, T, R]): Node[U, T, OutRank] = {
    val ld = Div(lhs.deriv(wrt), rhs)
    val rd = Div(Div(Mul(rhs.deriv(wrt), lhs), rhs), rhs)
    Sub(ld, rd)
  }

  override def propagate[R <: Nat, OutRank <: Nat](g: Node[U, T, R]): Node[U, T, OutRank] = {
    lhs.propagate(g / rhs) + rhs.propagate(-g * lhs / rhs / rhs)
  }
}

case class Pos[U[_], T, Rank <: Nat](v: Node[U, T, Rank])(implicit r: ValueRule[U, T]) extends UnaryOp[U, T, Rank, Rank] {
  override def toString: String = s"+(${ v })"
  override def apply(): Value[U, T] = +v()
  override def deriv[R <: Nat, OutRank <: Nat](wrt: Var[U, T, R]): Node[U, T, OutRank] = Pos(v.deriv(wrt))
  override def propagate[R <: Nat, OutRank <: Nat](g: Node[U, T, R]): Node[U, T, OutRank] = v.propagate(+g)
}

case class Neg[U[_], T, Rank <: Nat](v: Node[U, T, Rank])(implicit r: ValueRule[U, T]) extends UnaryOp[U, T, Rank, Rank] {
  override def toString: String = s"-(${ v })"
  override def apply(): Value[U, T] = -v()
  override def deriv[R <: Nat, OutRank <: Nat](wrt: Var[U, T, R]): Node[U, T, OutRank] = Neg(v.deriv(wrt))
  override def propagate[R <: Nat, OutRank <: Nat](g: Node[U, T, R]): Node[U, T, OutRank] = v.propagate(-g)
}

case class Transpose[U[_], T, Rank](v: Node[U, T, Rank])(implicit r: ValueRule[U, T]) extends UnaryOp[U, T, Rank, Rank] {
  override def toString: String = s"${ v }.T"
  override def apply(): Value[U, T] = v().T
  override def deriv[R <: Nat, OutRank <: Nat](wrt: Var[U, T, R]): Node[U, T, OutRank] = Transpose(v.deriv(wrt))
  override def propagate[R <: Nat, OutRank <: Nat](g: Node[U, T, R]): Node[U, T, OutRank] = v.propagate(g.T)
}
