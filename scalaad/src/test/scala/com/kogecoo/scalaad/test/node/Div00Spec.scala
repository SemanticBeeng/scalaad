package com.kogecoo.scalaad.test.node

import com.kogecoo.scalaad.graph._
import com.kogecoo.scalaad.impl.std.Implicits._
import com.kogecoo.scalaad.test.helper.impl.std.Implicits._
import com.kogecoo.scalaad.test.helper.impl.std.StdValueGen
import org.scalacheck.Prop.forAll
import org.scalacheck.Properties


object Div00Spec extends Properties("Div00") with NodeSpecBase {

  override val defaultMinValue = Some(-1e10)
  override val defaultMaxValue = Some( 1e10)

  val denomDomain = StdValueGen(
    defaultMinValue,
    defaultMaxValue,
    (x: Double) => x != -0.0 && x != 0.0
  )

  def op(a: N0, b: N0): N0 = Div00(a, b)

  property("eval") = forAll(genN0(), genNonzeroN0(denomDomain)) { (a: N0, b: N0) =>
    op(a, b).eval[T0] shouldCloseTo a.toStd / b.toStd
  }

  property("node0 / node0 forward w.r.t node0") = forAll(genN0(), genNonzeroN0(denomDomain), genN0()) { (a: N0, b: N0, c: N0) =>
    op(a, b).forward[N0, N0](c).eval[T0] shouldCloseTo 0.0
  }

  property("node0 / node0 forward w.r.t node1") = forAll(genN0(), genNonzeroN0(denomDomain), genN1()) { (a: N0, b: N0, c: N1) =>
    op(a, b).forward[N1, N1](c).eval[T1] shouldCloseTo zero1(c)
  }

  property("node0 / node0 forward w.r.t node2") = forAll(genN0(), genNonzeroN0(denomDomain), genN2()) { (a: N0, b: N0, c: N2) =>
    op(a, b).forward[N2, N2](c).eval[T2] shouldCloseTo zero2(c)
  }

  property("var0 / node0 forward w.r.t left") = forAll(genV0(), genNonzeroN0(denomDomain)) { (a: Var0, b: N0) =>
    op(a, b).forward[N0, N0](a).eval[T0] shouldCloseTo 1.0 / b.toStd
  }

  property("node0 / var0 forward w.r.t right") = forAll(genN0(), genV0(denomDomain)) { (a: N0, b: Var0) =>
    op(a, b).forward[N0, N0](b).eval[T0] shouldCloseTo -a.toStd / (b.toStd * b.toStd)
  }

  //  FIXME: the differentiation result of (var / var) should be zero
  // but following code doesn't produce it because of a floating-point precision issue.
  property("var0 / var0 forward w.r.t self") = forAll(genV0(denomDomain)) { (a: Var0) =>
    val x = a.toStd

    val l = 1 / x
    val r = (-x * 1) / (x * x)

    op(a, a).forward[N0, N0](a).eval[T0] shouldCloseTo l + r // 0.0
  }


  property("nonvar0 / nonvar0 reverse node0") = forAll(genNV0(), genNonzeroNV0(denomDomain), genN0()) { (a: N0, b: N0, c: N0) =>
    op(a, b).reverse(c).size == 0
  }

  property("nonvar0 / nonvar0 reverse node1") = forAll(genNV0(), genNonzeroNV0(denomDomain), genN1()) { (a: N0, b: N0, c: N1) =>
    op(a, b).reverse(c).size == 0
  }

  property("nonvar0 / nonvar0 reverse node2") = forAll(genNV0(), genNonzeroNV0(denomDomain), genN2()) { (a: N0, b: N0, c: N2) =>
    op(a, b).reverse(c).size == 0
  }

  property("var0 / nonvar0 reverse node0") = forAll(genV0(), genNonzeroNV0(denomDomain), genN0()) { (a: Var0, b: N0, c: N0) =>
    val g = op(a, b).reverse(c)
    g(a).get.asInstanceOf[N0].eval[T0] shouldCloseTo c.toStd / b.toStd
  }

  property("var0 / nonvar0 reverse node1") = forAll(genV0(), genNonzeroNV0(denomDomain), genN1()) { (a: Var0, b: N0, c: N1) =>
    val g = op(a, b).reverse(c)
    g(a).get.asInstanceOf[N1].eval[T1] shouldCloseTo (c.toStd div b.toStd)
  }

  property("var0 / nonvar0 reverse node2") = forAll(genV0(), genNonzeroNV0(denomDomain), genN2()) { (a: Var0, b: N0, c: N2) =>
    val g = op(a, b).reverse(c)
    g(a).get.asInstanceOf[N2].eval[T2] shouldCloseTo (c.toStd div b.toStd)
  }

  property("nonvar0 / var0 reverse node0") = forAll(genNV0(), genV0(denomDomain), genN0()) { (a: N0, b: Var0, c: N0) =>
    val g = op(a, b).reverse(c)
    g(b).get.asInstanceOf[N0].eval[T0] shouldCloseTo  -a.toStd * c.toStd / (b.toStd * b.toStd)
  }

  property("nonvar0 / var0 reverse node1") = forAll(genNV0(), genV0(denomDomain), genN1()) { (a: N0, b: Var0, c: N1) =>
    val g = op(a, b).reverse(c)
    g(b).get.asInstanceOf[N1].eval[T1] shouldCloseTo ((c.toStd.neg mul a.toStd) div (b.toStd * b.toStd))
  }

  property("nonvar0 / var0 reverse node2") = forAll(genNV0(), genV0(denomDomain), genN2()) { (a: N0, b: Var0, c: N2) =>
    val g = op(a, b).reverse(c)
    g(b).get.asInstanceOf[N2].eval[T2] shouldCloseTo ((c.toStd.neg mul a.toStd) div (b.toStd * b.toStd))
  }

  //  FIXME: the differentiation result of (var / var) should be zero
  // but following code doesn't produce it because of a floating-point precision issue.
  property("var0 / var0 reverse node0") = forAll(genV0(), genNonzeroN0(denomDomain)) { (a: Var0, b: N0) =>
    val g = op(a, a).reverse(b)

    val x = a.toStd
    val y = b.toStd

    val l = y / x
    val r = (-y * x) / (x * x)

    g(a).get.asInstanceOf[N0].eval[T0] shouldCloseTo l + r // 0.0
  }

  //  FIXME: the differentiation result of (var / var) should be zero
  // but following code doesn't produce it because of a floating-point precision issue.
  property("var0 / var0 reverse node1") = forAll(genV0(), genNonzeroN1(value = denomDomain)) { (a: Var0, b: N1) =>
    val g = op(a, a).reverse(b)

    val x = a.toStd
    val y = b.toStd

    val l = y div x
    val r = (y.neg mul x) div (x * x)

    g(a).get.asInstanceOf[N1].eval[T1] shouldCloseTo (l add r) // zero1(a)
  }

  //  FIXME: the differentiation result of (var / var) should be zero
  // but following code doesn't produce it because of a floating-point precision issue.
  property("var0 / var0 reverse node2") = forAll(genV0(), genNonzeroN2(value = denomDomain)) { (a: Var0, b: N2) =>
    val g = op(a, a).reverse(b)

    val x = a.toStd
    val y = b.toStd

    val l = y div x
    val r = (y.neg mul x) div (x * x)
    g(a).get.asInstanceOf[N2].eval[T2] shouldCloseTo (l add r) // zero2(b)

  }

}


