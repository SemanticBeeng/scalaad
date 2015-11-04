package com.kogecoo.scalaad.graph


import com.kogecoo.scalaad.Shape
import com.kogecoo.scalaad.algorithm.{Grad, Eval, Forward, Reverse}

// g -> adjoint
trait Node[S <: Shape] {
  val shape: S
  def forward[W, O](w: W)(implicit F: Forward[Node[S], W, O]): O = F.forward(this, w)
  def reverse[G](g: G)(implicit R: Reverse[Node[S], G]): Grad = R.reverse(this, g)

  def eval[V](implicit E: Eval[Node[S], V]): V = E.eval(this)
  def grad(implicit R: Reverse[Node[S], N0]): Grad = reverse[N0](One0())

}



object Node {

  implicit class Node0Op(val self: N0) extends AnyVal {
    def +(rhs: N0): Add00 = Add00(self, rhs)
    def -(rhs: N0): Sub00 = Sub00(self, rhs)
    def *(rhs: N0): Mul00 = Mul00(self, rhs)
    def /(rhs: N0): Div00 = Div00(self, rhs)

    def :+(rhs: N1): Add01 = Add01(self, rhs)
    def :-(rhs: N1): Sub01 = Sub01(self, rhs)
    def :*(rhs: N1): Mul01 = Mul01(self, rhs)
    def :/(rhs: N1): Div01 = Div01(self, rhs)

    def :+(rhs: N2): Add02 = Add02(self, rhs)
    def :-(rhs: N2): Sub02 = Sub02(self, rhs)
    def :*(rhs: N2): Mul02 = Mul02(self, rhs)
    def :/(rhs: N2): Div02 = Div02(self, rhs)

    def unary_+(): Pos0 = Pos0(self)
    def unary_-(): Neg0 = Neg0(self)

    def ==(rhs: N0): B0 = Eq00 (self, rhs)
    def !=(rhs: N0): B0 = Neq00(self, rhs)
    def < (rhs: N0): B0 = Lt00 (self, rhs)
    def <=(rhs: N0): B0 = Lte00(self, rhs)
    def > (rhs: N0): B0 = Gt00 (self, rhs)
    def >=(rhs: N0): B0 = Gte00(self, rhs)

    def :==(rhs: N1): B1 = Eq01 (self, rhs)
    def :!=(rhs: N1): B1 = Neq01(self, rhs)
    def :< (rhs: N1): B1 = Lt01 (self, rhs)
    def :<=(rhs: N1): B1 = Lte01(self, rhs)
    def :> (rhs: N1): B1 = Gt01 (self, rhs)
    def :>=(rhs: N1): B1 = Gte01(self, rhs)

    def :==(rhs: N2)(implicit d: DummyImplicit): B2 = Eq02 (self, rhs)
    def :!=(rhs: N2)(implicit d: DummyImplicit): B2 = Neq02(self, rhs)
    def :< (rhs: N2)(implicit d: DummyImplicit): B2 = Lt02 (self, rhs)
    def :<=(rhs: N2)(implicit d: DummyImplicit): B2 = Lte02(self, rhs)
    def :> (rhs: N2)(implicit d: DummyImplicit): B2 = Gt02 (self, rhs)
    def :>=(rhs: N2)(implicit d: DummyImplicit): B2 = Gte02(self, rhs)

  }

  implicit class Node1Op(val self: N1) extends AnyVal {

    def +(rhs: N1): Add11 = Add11(self, rhs)
    def -(rhs: N1): Sub11 = Sub11(self, rhs)
    def *(rhs: N1): Mul11 = Mul11(self, rhs)
    def /(rhs: N1): Div11 = Div11(self, rhs)

    def :+(rhs: N0): Add10 = Add10(self, rhs)
    def :-(rhs: N0): Sub10 = Sub10(self, rhs)
    def :*(rhs: N0): Mul10 = Mul10(self, rhs)
    def :/(rhs: N0): Div10 = Div10(self, rhs)

    def :+(rhs: N2)(implicit d: DummyImplicit): Add12 = Add12(self, rhs)
    def :-(rhs: N2)(implicit d: DummyImplicit): Sub12 = Sub12(self, rhs)
    def :*(rhs: N2)(implicit d: DummyImplicit): Mul12 = Mul12(self, rhs)
    def :/(rhs: N2)(implicit d: DummyImplicit): Div12 = Div12(self, rhs)

    def unary_+(): Pos1 = Pos1(self)
    def unary_-(): Neg1 = Neg1(self)
    def T: Transpose1 = Transpose1(self)

    def ==(rhs: N1): B1 = Eq11 (self, rhs)
    def !=(rhs: N1): B1 = Neq11(self, rhs)
    def < (rhs: N1): B1 = Lt11 (self, rhs)
    def <=(rhs: N1): B1 = Lte11(self, rhs)
    def > (rhs: N1): B1 = Gt11 (self, rhs)
    def >=(rhs: N1): B1 = Gte11(self, rhs)

    def :==(rhs: N0): B1 = Eq10 (self, rhs)
    def :!=(rhs: N0): B1 = Neq10(self, rhs)
    def :< (rhs: N0): B1 = Lt10 (self, rhs)
    def :<=(rhs: N0): B1 = Lte10(self, rhs)
    def :> (rhs: N0): B1 = Gt10 (self, rhs)
    def :>=(rhs: N0): B1 = Gte10(self, rhs)

    //def :==(rhs: N2): B2 = Eq12 (self, rhs)
    //def :!=(rhs: N2): B2 = Neq12(self, rhs)
    //def :< (rhs: N2): B2 = Lt12 (self, rhs)
    //def :<=(rhs: N2): B2 = Lte12(self, rhs)
    //def :> (rhs: N2): B2 = Gt12 (self, rhs)
    //def :>=(rhs: N2): B2 = Gte12(self, rhs)

  }

  implicit class Node2Op(val self: N2) extends AnyVal {

    def +(rhs: N2): Add22 = Add22(self, rhs)
    def -(rhs: N2): Sub22 = Sub22(self, rhs)
    def *(rhs: N2): Mul22 = Mul22(self, rhs)
    def /(rhs: N2): Div22 = Div22(self, rhs)

    def :+(rhs: N0): Add20 = Add20(self, rhs)
    def :-(rhs: N0): Sub20 = Sub20(self, rhs)
    def :*(rhs: N0): Mul20 = Mul20(self, rhs)
    def :/(rhs: N0): Div20 = Div20(self, rhs)

    def :+(rhs: N1)(implicit d: DummyImplicit): Add21 = Add21(self, rhs)
    def :-(rhs: N1)(implicit d: DummyImplicit): Sub21 = Sub21(self, rhs)
    def :*(rhs: N1)(implicit d: DummyImplicit): Mul21 = Mul21(self, rhs)
    def :/(rhs: N1)(implicit d: DummyImplicit): Div21 = Div21(self, rhs)

    def unary_+(): Pos2 = Pos2(self)
    def unary_-(): Neg2 = Neg2(self)
    def T: Transpose2 = Transpose2(self)

    def ==(rhs: N2): B2 = Eq22 (self, rhs)
    def !=(rhs: N2): B2 = Neq22(self, rhs)
    def < (rhs: N2): B2 = Lt22 (self, rhs)
    def <=(rhs: N2): B2 = Lte22(self, rhs)
    def > (rhs: N2): B2 = Gt22 (self, rhs)
    def >=(rhs: N2): B2 = Gte22(self, rhs)

    def :==(rhs: N0): B2 = Eq20 (self, rhs)
    def :!=(rhs: N0): B2 = Neq20(self, rhs)
    def :< (rhs: N0): B2 = Lt20 (self, rhs)
    def :<=(rhs: N0): B2 = Lte20(self, rhs)
    def :> (rhs: N0): B2 = Gt20 (self, rhs)
    def :>=(rhs: N0): B2 = Gte20(self, rhs)

    //def :==(rhs: N1): B2 = Eq21 (self, rhs)
    //def :!=(rhs: N1): B2 = Neq21(self, rhs)
    //def :< (rhs: N1): B2 = Lt21 (self, rhs)
    //def :<=(rhs: N1): B2 = Lte21(self, rhs)
    //def :> (rhs: N1): B2 = Gt21 (self, rhs)
    //def :>=(rhs: N1): B2 = Gte21(self, rhs)

  }

}

