package com.kogecoo.scalaad.graph

import com.kogecoo.scalaad.rule.ValueRule
import com.kogecoo.scalaad.value.Value

import shapeless.Nat
import shapeless.ops.nat.LT.<
import shapeless.ops.nat.{Max, Diff, Min}

import scala.language.higherKinds


// The most fundamental component of computational graph for automatic derivation.
// Every node in a computational graph must inherit Node.
//
//  Node doesn't know actual computational rule (how to calculate +, -, *, ...),
// so if you want to compute derivation on your own class (ComplexNumber, Matrix, etc),
// you need to define its computational rules (see the definition of ValueRule).

trait Node[U[_], T, Rank <: Nat] {

  type N = Node[U, T, Rank]
  type N0 = Node[U, T, Nat._0]
  type NK[K] = Node[U, T, K]

  override def toString: String

  def apply(): Value[U, T]
  def deriv[K <: Nat](wrt: Var[U, T, K]): N // compute with forward-mode automatic differentiation
  def propagate0[K <: Nat : Eq0[K]](g: Node[U, T, K]): N    // compute with reverse-mode autmatic differentiation
  def propagate1[K <: Nat : Eq1[K]](g: Node[U, T, K]): N
  def propagate2[K <: Nat : Eq2[K]](g: Node[U, T, K]): N
  def grad[K <: Nat](fixme: U[T])(implicit r: ValueRule[U, T]): Node[U, T, OutRank] = propagate(One[U, T, OutRank](fixme))

  //propagate0, 1, 2
}


object Node {

  implicit class NodeOp[U[_], T, L <: Nat](self: Node[U, T, L]) {
    type N[Rank] = Node[U, T, Rank]
    type EqL[R] = L =:= R
    type ZeroL = L =:= Nat._0
    type Gt0[R] = Nat._0 < R

    /*def +[R <: Nat](rhs: N[R])(implicit ev: Max[L, R]): N[ev.Out] = {
      (self, rhs) match {
        case (l: Nat._0, r: Nat._0) => Add00[U, T, Nat._0, Nat._0](l, r)
        case (l: Nat   , r: Nat._0) => AddL0(l, r)
        case (l: Nat._0, r: Nat   ) => Add0R(l, r)
        case (l: Nat   , r: Nat   ) => AddLR(l, r)
      }
    }*/
    def +[R <: Nat : Eq0](rhs: N[R])(implicit ev: Eq0[L]): N[L] = Add00[U, T, L, R](self, rhs)
    def +[R <: Nat : Eq0](rhs: N[R])(implicit ev: Gt0[L]): N[L] = AddL0[U, T, L, R](self, rhs)
    def +[R <: Nat : Gt0](rhs: N[R])(implicit ev: ZeroL):  N[R] = Add0R[U, T, L, R](self, rhs)
    def +[R <: Nat : Gt0 : EqL](rhs: N[R])(implicit ev1: Gt0[L]): N[L] = AddLR[U, T, L, R](self, rhs)



    def -[RankR <: Nat](rhs: Node[U, T, RankR])(implicit c: Cons[RankR], r: ValueRule[U, T], o: OutRank[RankR]): Node[U, T, o.Out] = Sub[U, T, L, RankR, o.Out](self, rhs)
    def *[RankR <: Nat](rhs: Node[U, T, RankR])(implicit c: Cons[RankR], r: ValueRule[U, T], o: OutRank[RankR]): Node[U, T, o.Out] = Mul[U, T, L, RankR, o.Out](self, rhs)
    def /[RankR <: Nat](rhs: Node[U, T, RankR])(implicit c: Cons[RankR], r: ValueRule[U, T], o: OutRank[RankR]): Node[U, T, o.Out] = Div[U, T, L, RankR, o.Out](self, rhs)

    def unary_+(rhs: Node[U, T, L])(implicit r: ValueRule[U, T]): Node[U, T, L] = Pos[U, T, L](self)
    def unary_-(rhs: Node[U, T, L])(implicit r: ValueRule[U, T]): Node[U, T, L] = Neg[U, T, L](self)

    def T(implicit r: ValueRule[U, T]): Node[U, T, L] = Transpose[U, T, L](self)
  }

}
