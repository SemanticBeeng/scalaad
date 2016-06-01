package com.kogecoo.scalaad.graph

import com.kogecoo.scalaad.analyze.{Analyzing, ElementwiseEqn2, Eqn1, Eqn2, FillEqn1, FoldEqn1, FoldEqn2, Param, WhereEqn}
import com.kogecoo.scalaad.graph.bool.BooleanExpr
import com.kogecoo.scalaad.op.{BinaryOp, Op0, Op00, UnaryOp}
import com.kogecoo.scalaad.{S0, Shape, Shape0}


/**
  * represents applying binary operation, which takes 2 Exprs arguments.
  *
  * @tparam S a shape of this Expr
  * @tparam L a shape of left Expr
  * @tparam R a shape of right Expr
  */
trait Application2[S <: Shape, L <: Shape, R <: Shape] extends ValueExpr[S] {
  def shape: S
  def l: ValueExpr[L]
  def r: ValueExpr[R]
}

/**
  * specialized Application2, its left and output Expr are the same type of shape.
  *
  * @tparam L a shape of left and output Expr
  * @tparam R a shape of right Expr
  */
trait LeftShapedApplication2[L <: Shape, R <: Shape] extends Application2[L, L, R] {
  def shape: L = l.shape
  def l: ValueExpr[L]
  def r: ValueExpr[R]
}

/**
  * specialized Application2, its right and output Expr are the same type of shape.
  *
  * @tparam L a shape of left Expr
  * @tparam R a shape of right and output Expr
  */
trait RightShapedApplication2[L <: Shape, R <: Shape] extends Application2[R, L, R] {
  def shape: R = r.shape
  def l: ValueExpr[L]
  def r: ValueExpr[R]
}

/**
  * specialized Application2, its left, right and output Expr are the same type of shape.
  *
  * @tparam S type of shape for left, right and output Expr
  */
trait CommonShapedApplication2[S <: Shape] extends Application2[S, S, S] {
  def shape: S = l.shape
  def l: ValueExpr[S]
  def r: ValueExpr[S]
}

/**
  * represents applying unary operation, which takes 1 Expr as argument.
  *
  * @tparam O type of shape for output Expr
  * @tparam S type of shape for argument Expr
  */
trait Application1[O <: Shape, S <: Shape] extends ValueExpr[O] {
  def shape: O
  def v: ValueExpr[S]
}

/**
  * specialized Application1, its input and output Expr are the same type of shape.
  *
  * @tparam S type of shape for left, right and output Expr
  */
trait CommonShapedApplication1[S <: Shape] extends Application1[S, S] {
  def shape: S = v.shape
  def v: ValueExpr[S]
}


// Unary Application

case class Apply1[S <: Shape](v: VE[S], op: Op0) extends CommonShapedApplication1[S] {

  def analyze(a: Analyzing): Param[S] = a.addEqn(Eqn1(v.analyze(a), op))

  // df(g(x)) / dx = f'(g(x)) * f(g'(x))
  def forward[SO <: Shape, SI <: Shape](wrt: VarBase[SI]): ValueExpr[SO] = {
    v.forward[SO, SI](wrt) * Apply1[SO, SI](v, op.deriv())
  }

}


case class ReshapeApply1[SO <: Shape, SI <: Shape](v: VE[SI], op: UnaryOp[SO, SI]) extends Application1[SO, SI] {

  def analyze(a: Analyzing): Param[SO] = a.addEqn(FoldEqn1(v.analyze(a), op))

  def forward[O <: Shape, I <: Shape](wrt: VarBase[I]): ValueExpr[O] = {
     ReshapeApply1[O, I](v.forward[O, I](wrt), op.deriv())
  }

}

// Binary Application

case class Apply2[S <: Shape](l: VE[S], r: VE[S], op: Op00) extends CommonShapedApplication2[S] {

  def analyze(a: Analyzing): Param[S] = a.addEqn(Eqn2(l.analyze(a), r.analyze(a), op))

  def forward[SO <: Shape, SI <: Shape](wrt: VarBase[SI]): ValueExpr[SO] = {
     Apply2[SO, SI](l.forward[SO, SI](wrt), r, op.deriv()) + Apply2[SO, SI](l, r.forward[SO, SI](wrt), op.deriv())
  }

}


case class ElementwiseLeft[SI1 <: Shape, SI2 <: Shape](l: VE[SI1], r: VE[SI2], op: Op00) extends LeftShapedApplication2[SI1, SI2] {

  def analyze(a: Analyzing): Param[SI1] = a.addEqn(ElementwiseEqn2(l.analyze(a), r.analyze(a), op))
}


case class ElementwiseRight[SI1 <: Shape, SI2 <: Shape](l: VE[SI1], r: VE[SI2], op: Op00) extends RightShapedApplication2[SI1, SI2] {

  def analyze(a: Analyzing): Param[SI2] = a.addEqn(ElementwiseEqn2(l.analyze(a), r.analyze(a), op))
}


case class ReshapeApply2[SO <: Shape, SI1 <: Shape, SI2 <: Shape](l: VE[SI1], r: VE[SI2], op: BinaryOp[SO, SI1, SI2]) extends Application2[SO, SI1, SI2] {

  def analyze(a: Analyzing): Param[SO] = a.addEqn(FoldEqn2(l.analyze(a), r.analyze(a), op))

}


case class Where[S <:  Shape](cond: BooleanExpr[S], l: VE[S], r: VE[S]) extends CommonShapedApplication2[S] {

  def analyze(a: Analyzing): Param[S] = a.addEqn(WhereEqn[S](cond, l.analyze(a), r.analyze(a)))
}
