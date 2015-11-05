package com.kogecoo.scalaad.graph

import com.kogecoo.scalaad.graph.op._
import com.kogecoo.scalaad.rule.ValueRule

import shapeless.Nat

import scala.language.higherKinds


trait Node[Rank] {

  override def toString: String

  def grad: Node[U, T, OutRank] = propagate(One[U, T, OutRank](fixme))

  //propagate0, 1, 2
}

trait Node0[T] extends Node[Nat._0] {

  def apply(): T

  def deriv0      (wrt: N0[T]   ): N0[T]
  def deriv1[V[_]](wrt: N1[V, T]): N1[V, T]
  def deriv2[M[_]](wrt: N2[M, T]): N2[M, T]

  def propagate0      (g: N0[T]   ): N0[T]
  def propagate1[V[_]](g: N1[V, T]): N1[V, T]
  def propagate2[M[_]](g: N2[M, T]): N2[M, T]

}


trait Node1[V[_], T] extends Node[Nat._1] {

  def apply(): V[T]

  def deriv0      (wrt: N0[T]   ): N1[V, T]
  def deriv1[M[_]](wrt: N1[V, T]): N2[M, T]

  def propagate0(g: N0[T]): N1[V, T]
  def propagate1[M[_]](g: N1[V, T]): N2[M, T]

}

trait Node2[M[_], T] extends Node[Nat._2] {

  def apply(): M[T]

  def deriv0(wrt: N0[T]): N2[M, T]

  def propagate0(g: N0[T]): N2[M, T]

}


object Node {

  implicit class Node0Ops[T](self: N0[T]) {

    def +(rhs: N0[T]): N0[T] = Add00[T](self, rhs)

    def :+[V[_]](rhs: N1[V, T]): N1[V, T] = BroadcastAdd01[V, T](self, rhs)
    def :+[M[_]](rhs: N2[M, T]): N2[M, T] = BroadcastAdd02[M, T](self, rhs)

    def -(rhs: N0[T]): N0[T] = Sub00[T](self, rhs)

    def :-[V[_]](rhs: N1[V, T]): N1[V, T] = BroadcastSub01[V, T](self, rhs)
    def :-[M[_]](rhs: N2[M, T]): N2[M, T] = BroadcastSub02[M, T](self, rhs)

    def *(rhs: N0[T]): N0[T] = Mul00[T](self, rhs)

    def :*[V[_]](rhs: N1[V, T]): N1[V, T] = BroadcastMul01[V, T](self, rhs)
    def :*[M[_]](rhs: N2[M, T]): N2[M, T] = BroadcastMul02[M, T](self, rhs)

    def /(rhs: N0[T]): N0[T] = Div00[T](self, rhs)

    def :/[V[_]](rhs: N1[V, T]): N1[V, T] = BroadcastDiv01[V, T](self, rhs)
    def :/[M[_]](rhs: N2[M, T]): N2[M, T] = BroadcastDiv02[M, T](self, rhs)

    def unary_+(): N0[T] = Pos0[T](self)
    def unary_-(): N0[T] = Neg0[T](self)
    def T(): N0[T] = Transpose0[T](self)
  }


  implicit class Node1Ops[V, T](self: N1[V, T]) {

    def +(rhs: N1[V, T]): N1[V, T] = Add11[V, T](self, rhs)

    def :+(rhs: N0[T]   ): N1[V, T] = BroadcastAdd10[V, T](self, rhs)
    def :+[M[_]](rhs: N2[M, T]): N2[M, T] = BroadcastAdd12[M, V, T](self, rhs)

    def -(rhs: N1[V, T]): N1[V, T] = Sub11[V, T](self, rhs)

    def :-(rhs: N0[T]   ): N1[V, T] = BroadcastSub10[V, T](self, rhs)
    def :-[M[_]](rhs: N2[M, T]): N2[M, T] = BroadcastSub12[M, V, T](self, rhs)

    def *(rhs: N1[V, T]): N1[V, T] = Mul11[V, T](self, rhs)

    def :*(rhs: N0[T]   ): N1[V, T] = BroadcastMul10[V, T](self, rhs)
    def :*[M[_]](rhs: N2[M, T]): N2[M, T] = BroadcastMul12[M, V, T](self, rhs)

    def /(rhs: N1[V, T]): N1[V, T] = Div11[V, T](self, rhs)

    def :/(rhs: N0[T]   ): N1[V, T] = BroadcastDiv10[V, T](self, rhs)
    def :/[M[_]](rhs: N2[M, T]): N2[M, T] = BroadcastDiv12[M, V, T](self, rhs)

    def unary_+(): N1[V, T] = Pos1[V, T](self)
    def unary_-(): N1[V, T] = Neg1[V, T](self)
    def T(): N1[V, T] = Transpose1[V, T](self)
  }


  implicit class Node2Ops[M, T](self: N2[M, T]) {

    def +(rhs: N2[M, T]): N2[M, T] = Add22[M, T](self, rhs)

    def :+[V[_]](rhs: N1[V, T]): N2[M, T] = BroadcastAdd21[M, V, T](self, rhs)
    def :+(rhs: N0[T]   ): N2[M, T] = BroadcastAdd20[M, T](self, rhs)

    def -(rhs: N2[M, T]): N2[M, T] = Sub22[M, T](self, rhs)

    def :-[V[_]](rhs: N1[V, T]): N2[M, T] = BroadcastSub21[M, V, T](self, rhs)
    def :-(rhs: N0[T]   ): N2[M, T] = BroadcastSub20[M, T](self, rhs)

    def *(rhs: N2[M, T]): N2[M, T] = Mul22[M, T](self, rhs)

    def :*[V[_]](rhs: N1[V, T]): N2[M, T] = BroadcastMul21[M, V, T](self, rhs)
    def :*(rhs: N0[T]   ): N2[M, T] = BroadcastMul20[M, T](self, rhs)

    def /(rhs: N2[M, T]): N2[M, T] = Div22[M, T](self, rhs)

    def :/[V[_]](rhs: N1[V, T]): N2[M, T] = BroadcastDiv21[M, V, T](self, rhs)
    def :/(rhs: N0[T]   ): N2[M, T] = BroadcastDiv20[M, T](self, rhs)

    def unary_+(): N2[M, T] = Pos2[M, T](self)
    def unary_-(): N2[M, T] = Neg2[M, T](self)
    def T(): N2[M, T] = Transpose2[M, T](self)
  }

}
