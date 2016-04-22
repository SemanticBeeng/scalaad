package com.kogecoo.scalaad.test.node.unary

import com.kogecoo.scalaad.node.{Exp1, N1}
import com.kogecoo.scalaad.test.helper.impl.std.Implicits._
import com.kogecoo.scalaad.test.{SpecBackend, StdSpecBackend}
import org.scalacheck.Properties


object StdExp1Spec extends Properties("Exp1") with Exp1Spec with StdSpecBackend {

  override def expectApplyOp(a: N1): T1 = broadcast1(a.toT1, math.exp)

  override def deriv(a: T0): T0 = math.exp(a)

  override def defaultMinValue = Some(-100.0)

  override def defaultMaxValue = Some(100.0)
}


trait Exp1Spec extends UnaryOp1SpecBase { self: Properties with SpecBackend =>


  override def op(a: N1): N1 = Exp1(a)

  override def op(argStr: String): String = s"exp($argStr)"

}
