package com.kogecoo.scalaad.graph

import com.kogecoo.scalaad.Constraint
import com.kogecoo.scalaad.Shape
import com.kogecoo.scalaad.op.{BinaryFoldOp, BinaryOp}
import shapeless.{Nat, Succ}

import scala.language.existentials


/**
  * represents applying binary operation, which takes 2 Exprs arguments.
  *
  * @tparam N a shape of this Expr
  * @tparam L a shape of left Expr
  * @tparam R a shape of right Expr
  */
trait Application2[N <: Nat, L <: Nat, R <: Nat] extends V[N] {

  def shape: Shape[N]

  def l: V[L]

  def r: V[R]

}

// Binary Application

abstract class ElementwiseApply2Base[N <: Nat, L <: Nat, R <: Nat](l: V[L], r: V[R], op: BinaryOp) extends Application2[N, L, R] {

  def shape: Shape[N] = (l.shape, r.shape) match {
    case (ls, rs) if ls.order >= rs.order => ls.asInstanceOf[Shape[N]]
    case (ls, rs)                         => rs.asInstanceOf[Shape[N]]
  }

  type LO <: Nat

  type RO <: Nat

  def _forward[W <: Nat, O <: Nat](wrt: V[W]): V[O] = {
    val (dl, dr) = op.deriv[L, R](l, r)
    val fl: V[LO] = l._forward[W, LO](wrt)
    val fr: V[RO] = r._forward[W, RO](wrt)
    (fl :* dr) :+ (dl :* fr)
  }

  def _reverse[G <: Nat](adj: V[G]): Grad[G] = {
    val (dl, dr) = op.deriv[L, R](l, r)
    l._reverse[G](adj  :* dr) ++ r._reverse[G](dl :* adj)
  }

}


object InferElementwise2 {

    def apply[O <: Nat, X <: Nat, Y <: Nat](a: V[X], b: V[Y], op: BinaryOp): V[O] = {
    (a, b) match {
      case _ if a.shape.order == b.shape.order => {
        val a_ = a.asInstanceOf[V[O]]
        val b_ = b.asInstanceOf[V[O]]
        Elementwise2[O](a_, b_, op)
      }
      case _ if a.shape.order > b.shape.order => {
        val a_ = a.asInstanceOf[V[O]]
        BroadcastLeft2[O, Y](a_, b, op)
      }
      case _ => {
        val b_ = b.asInstanceOf[V[O]]
        BroadcastRight2[X, O](a, b_, op)
      }
    }
  }

}


@throws[Exception]
case class Elementwise2[N <: Nat](l: V[N], r: V[N], op: BinaryOp) extends ElementwiseApply2Base[N, N, N](l, r, op) {

  Constraint.commonShape(l, r)

  override def shape: Shape[N] = l.shape

}


@throws[Exception]
case class BroadcastLeft2[L <: Nat, R <: Nat](l: V[L], r: V[R], op: BinaryOp) extends ElementwiseApply2Base[L, L, R](l, r, op) {

  Constraint.leftOrderBiggerThanRight(l, r)

  Constraint.broadcastableToLeft(l, r)

  override def shape: Shape[L] = l.shape

}


@throws[Exception]
case class BroadcastRight2[L <: Nat, R <: Nat](l: V[L], r: V[R], op: BinaryOp) extends ElementwiseApply2Base[R, L, R](l, r, op) {

  Constraint.rightOrderBiggerThanLeft(l, r)

  Constraint.broadcastableToRight(l, r)

  override def shape: Shape[R] = r.shape

}


abstract class Fold2Base[N <: Nat, L <: Nat, R <: Nat](l: V[L], r: V[R], op: BinaryFoldOp, axis: Int) extends Application2[N, L, R] {

  type LO <: Nat

  type RO <: Nat

  @throws[Exception]
  def shape: Shape[N] = (l.shape, r.shape) match {
    case (ls, rs) if ls.order >= rs.order => ls.shrink(List(axis)).asInstanceOf[Shape[N]]
    case (ls, rs)                         => rs.shrink(List(axis)).asInstanceOf[Shape[N]]
  }

  def _forward[W <: Nat, O <: Nat](wrt: V[W]): V[O] = {
    val (dl, dr) = op.deriv(l, r)
    val fl = l._forward[W, LO](wrt)
    val fr = r._forward[W, RO](wrt)
    InferFold2(fl, dr, op, axis) :+ InferFold2(dl, fr, op, axis)
  }

  def _reverse[G <: Nat](adj: V[G]): Grad[G] = {
    val (dl, dr) = op.deriv(l, r)
    l._reverse[G](adj :* InferFold2(dl, r, op, axis)) ++ r._reverse[G](adj :* InferFold2(l, dr, op, axis))
  }

}


object InferFold2 {

  def apply[O <: Nat, X <: Nat, Y <: Nat](a: V[X], b: V[Y], op: BinaryFoldOp, axis: Int): V[O] = {
    (a, b) match {
      case _ if a.shape.order == b.shape.order => {
        val a_ = a.asInstanceOf[V[Succ[O]]]
        val b_ = b.asInstanceOf[V[Succ[O]]]
        Fold2[O](a_, b_, op, axis)
      }
      case _ if a.shape.order > b.shape.order => {
        val a_ = a.asInstanceOf[V[Succ[O]]]
        BroadcastLeftFold2[O, Y](a_, b, op, axis)
      }
      case _ => {
        val b_ = b.asInstanceOf[V[Succ[O]]]
        BroadcastRightFold2[X, O](a, b_, op, axis)
      }
    }
  }

}


@throws[Exception]
case class Fold2[N <: Nat](l: V[Succ[N]], r: V[Succ[N]], op: BinaryFoldOp, axis: Int) extends Fold2Base[N, Succ[N], Succ[N]](l, r, op, axis) {

  Constraint.validAxis(l, axis)

  Constraint.commonShape(l, r)

  override def shape: Shape[N] = l.shape.shrink(List(axis))

}


@throws[Exception]
case class BroadcastLeftFold2[L <: Nat, R <: Nat](l: V[Succ[L]], r: V[R], op: BinaryFoldOp, axis: Int) extends Fold2Base[L, Succ[L], R](l, r, op, axis) {

  Constraint.validAxis(l, axis)

  Constraint.validAxis(r, axis)

  Constraint.leftOrderBiggerThanRight(l, r)

  Constraint.broadcastableToLeft(l, r)

  override def shape: Shape[L] = l.shape.shrink(List(axis))

}


@throws[Exception]
case class BroadcastRightFold2[L <: Nat, R <: Nat](l: V[L], r: V[Succ[R]], op: BinaryFoldOp, axis: Int) extends Fold2Base[R, L, Succ[R]](l, r, op, axis) {

  Constraint.validAxis(l, axis)

  Constraint.validAxis(r, axis)

  Constraint.rightOrderBiggerThanLeft(l, r)

  Constraint.broadcastableToRight(l, r)

  override def shape: Shape[R] = l.shape.shrink(List(axis))

}
