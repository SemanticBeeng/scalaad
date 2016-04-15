package com.kogecoo.scalaad.test.node.unary

import com.kogecoo.scalaad.graph.{Abs0, N0}
import com.kogecoo.scalaad.test.{SpecBackend, StdSpecBackend}
import org.scalacheck.Properties


object StdAbs0Spec extends Properties("Abs0") with Abs0Spec with StdSpecBackend {

  import com.kogecoo.scalaad.test.helper.impl.std.Implicits._

  override def expectApplyOp(a: N0): T0 = math.abs(a.toT0)

  override def deriv(a: T0): T0 = 1.0

}

trait Abs0Spec extends UnaryOp0SpecBase { self: Properties with SpecBackend =>

  override def op(a: N0): N0 = Abs0(a)

  override def op(argStr: String): String = s"abs($argStr)"

}


