package com.kogecoo.scalaad.test.node

import com.kogecoo.scalaad.graph._
import com.kogecoo.scalaad.impl.std.Implicits._
import com.kogecoo.scalaad.test.helper.impl.std.Implicits._
import org.scalacheck.Prop.forAll
import org.scalacheck.Properties


object Atan1Spec extends Properties("Atan1") with UnaryOp1SpecBase {

  private[this] def deriv(x: Double): Double = 1.0 / (1.0 + x * x)

  override def defaultMinValue = Some(-1e15)
  override def defaultMaxValue = Some( 1e15)

  override def op(a: N1): N1 = Atan1(a)

  override def op(argStr: String): String = s"atan($argStr)"

  property("eval") = forAll(genN1()) { (a: N1) =>
    op(a).eval[T1] shouldCloseTo a.toStd.map(math.atan)
  }

  property(s"${op("var1")} forward w.r.t self") = forAll(genV1()) { (a: Var1) =>
    op(a).forward[N1, N2](a).eval[T2] shouldCloseTo diag(a.toStd.map(deriv))
  }

  property(s"${op("var1")} reverse node0") = forAll(genV1(), genN0()) { (a: Var1, b: N0) =>
    val g = op(a).reverse(b)
    g(a).get.asInstanceOf[N1].eval[T1] shouldCloseTo (a.toStd.map(deriv) mul b.toStd)
  }

  property(s"${op("var1")} reverse node1") = forAll(genV1_N1()) { case (a: Var1, b: N1) =>
    val g = op(a).reverse(b)
    g(a).get.asInstanceOf[N1].eval[T1] shouldCloseTo (a.toStd.map(deriv) mul b.toStd)
  }

  property(s"${op("var1")} reverse node2") = forAll(genV1_RowEquivN2()) { case (a: Var1, b: N2) =>
    val g = op(a).reverse(b)
    g(a).get.asInstanceOf[N2].eval[T2] shouldCloseTo (b.toStd colMul a.toStd.map(deriv))
  }

}
