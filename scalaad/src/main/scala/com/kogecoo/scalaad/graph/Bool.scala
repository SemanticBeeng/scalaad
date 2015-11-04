package com.kogecoo.scalaad.graph

import com.kogecoo.scalaad._
import com.kogecoo.scalaad.algorithm.Eval

trait BoolNode[S <: Shape] {

  val shape: S

  def eval[V](implicit E: Eval[BoolNode[S], V]): V = E.eval(this)

}

object BoolNode {

  implicit class BoolNode0Op(val self: B0) extends AnyVal {

    def &(rhs: B0): B0 = And00(self, rhs)
    def |(rhs: B0): B0 = Or00 (self, rhs)

    def :&(rhs: B1): B1 = And01(self, rhs)
    def :|(rhs: B1): B1 = Or01 (self, rhs)

    def :&(rhs: B2)(implicit d: DummyImplicit): B2 = And02(self, rhs)
    def :|(rhs: B2)(implicit d: DummyImplicit): B2 = Or02 (self, rhs)

    def unary_!(): B0 = Not0(self)

  }

  implicit class BoolNode1Op(val self: B1) extends AnyVal {

    def &(rhs: B1): B1 = And11(self, rhs)
    def |(rhs: B1): B1 = Or11 (self, rhs)

    def :&(rhs: B0): B1 = And10(self, rhs)
    def :|(rhs: B0): B1 = Or10 (self, rhs)

    //def :&(rhs: B2): B2 = And12(self, rhs)
    //def :|(rhs: B2): B2 = Or12 (self, rhs)

    def unary_!(): B1 = Not1(self)

  }

  implicit class BoolNode2Op(val self: B2) extends AnyVal {

    def &(rhs: B2): B2 = And22(self, rhs)
    def |(rhs: B2): B2 = Or22 (self, rhs)

    def :&(rhs: B0): B2 = And20(self, rhs)
    def :|(rhs: B0): B2 = Or20 (self, rhs)

    //def :&(rhs: B1): B2 = And21(self, rhs)
    //def :|(rhs: B1): B2 = Or21 (self, rhs)

    def unary_!(): B2 = Not2(self)

  }

}

case class Bool0(data: BoolTensor0) extends B0 { override val shape: S0 = Shape0() }

case class Bool1(data: BoolTensor1, shape: S1) extends B1

case class Bool2(data: BoolTensor2, shape: S2) extends B2

