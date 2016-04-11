package com.kogecoo.scalaad.test.node

import com.kogecoo.scalaad.graph._
import com.kogecoo.scalaad.impl.std.Implicits._
import com.kogecoo.scalaad.test.helper.impl.std.Implicits._
import org.scalacheck.Prop.forAll
import org.scalacheck.Properties


object Tan0Spec extends Properties("Tan0") with UnaryOp0SpecBase {

  override def defaultMinValue = Some(-1e10)
  override def defaultMaxValue = Some(1e10)

  override def op(a: N0): N0 = Tan0(a)

  override def op(argStr: String): String = s"tan($argStr)"

  property("eval") = forAll(genN0()) { (a: N0) =>
    op(a).eval[T0] shouldCloseTo math.tan(a.toStd)
  }

  property(s"${op("var0")} forward w.r.t self") = forAll(genV0()) { (a: Var0) =>
    val aTan = math.tan(a.toStd)
    op(a).forward[N0, N0](a).eval[T0] shouldCloseTo 1 + aTan * aTan
  }

  property(s"${op("var0")} reverse node0") = forAll(genV0(), genN0()) { (a: Var0, b: N0) =>
    val aTan = math.tan(a.toStd)
    val g = op(a).reverse(b)
    g(a).get.asInstanceOf[N0].eval[T0] shouldCloseTo b.toStd * (1 + aTan * aTan)
  }

  property(s"${op("var0")} reverse node1") = forAll(genV0(), genN1()) { (a: Var0, b: N1) =>
    val aTan = math.tan(a.toStd)
    val g = op(a).reverse(b)
    g(a).get.asInstanceOf[N1].eval[T1] shouldCloseTo (b.toStd mul (1 + aTan * aTan))
  }

  property(s"${op("var0")} reverse node2") = forAll(genV0(), genN2()) { (a: Var0, b: N2) =>
    val aTan = math.tan(a.toStd)
    val g = op(a).reverse(b)
    g(a).get.asInstanceOf[N2].eval[T2] shouldCloseTo (b.toStd mul (1 + aTan * aTan))
  }

}


