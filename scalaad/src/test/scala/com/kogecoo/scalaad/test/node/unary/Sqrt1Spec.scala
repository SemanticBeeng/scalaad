package com.kogecoo.scalaad.test.node.unary

import com.kogecoo.scalaad.graph.{N1, Sqrt1}
import com.kogecoo.scalaad.test.{SpecBackend, StdSpecBackend}
import org.scalacheck.Properties


object StdSqrt1Spec extends Properties("Sqrt1") with Sqrt1Spec with StdSpecBackend {

  import com.kogecoo.scalaad.test.helper.impl.std.Implicits._

  override def expectApplyOp(a: N1): T1 = broadcast1(a.toT1, math.sqrt)

  override def deriv(a: T0): T0 = 1 / (2 * math.sqrt(a))

  override def defaultMinValue = Some(0.0)

  override def defaultMaxValue = Some(100.0)

  override def defaultValueConstraint = (x: Double) => x != -0.0 && x != 0.0

}

trait Sqrt1Spec extends UnaryOp1SpecBase { self: Properties with SpecBackend =>

  override def op(a: N1): N1 = Sqrt1(a)

  override def op(argStr: String): String = s"sqrt($argStr)"

}


