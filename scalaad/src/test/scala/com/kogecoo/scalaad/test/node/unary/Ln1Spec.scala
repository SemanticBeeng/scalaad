package com.kogecoo.scalaad.test.node.unary

import com.kogecoo.scalaad.graph.{Ln1, N1}
import com.kogecoo.scalaad.test.helper.impl.std.Implicits._
import com.kogecoo.scalaad.test.{SpecBackend, StdSpecBackend}
import org.scalacheck.Properties


object StdLn1Spec extends Properties("Ln1") with Ln1Spec with StdSpecBackend {

  override def expectApplyOp(a: N1): T1 = broadcast1(a.toT1, math.log)

  override def deriv(a: T0): T0 = 1 / a

  override def defaultMinValue = Some(0.0)

  override def defaultMaxValue = Some(100.0)

  override def defaultValueConstraint = (x: Double) => x != -0.0 && x != 0.0

}


trait Ln1Spec extends UnaryOp1SpecBase { self: Properties with SpecBackend =>

  override def op(a: N1): N1 = Ln1(a)

  override def op(argStr: String): String = s"ln($argStr)"

}


