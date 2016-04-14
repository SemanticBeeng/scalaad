package com.kogecoo.scalaad.test.node.binary

import com.kogecoo.scalaad.graph._
import com.kogecoo.scalaad.test.helper.impl.std.StdValueGen
import com.kogecoo.scalaad.test.{SpecBackend, StdSpecBackend}
import org.scalacheck.{Gen, Properties}


object StdDiv00Spec extends Properties("Div00") with Div00Spec with StdSpecBackend {

  import com.kogecoo.scalaad.test.helper.impl.std.Implicits._

  override def expectApplyOp(a: N0, b: N0): T0 = a.toT0 / b.toT0

  override def leftDeriv(a: T0, b: T0): T0 = 1.0 / b

  override def rightDeriv(a: T0, b: T0): T0 = a / (b * b)

  override def leftRightDeriv(a: T0): T0 = 2.0

  override def defaultMinValue = Some(-1e10)

  override def defaultMaxValue = Some( 1e10)

  override def denomDomain: Gen[T0] = StdValueGen(
    defaultMinValue,
    defaultMaxValue,
    (x: Double) => x != -0.0 && x != 0.0
  )

  /*
  //  FIXME: the differentiation result of (var / var) should be zero
  // but following code doesn't produce it because of a floating-point precision issue.
  property("var0 / var0 reverse node0") = forAll(genV0(), genNonzeroN0(denomDomain)) { (a: Var0, b: N0) =>
    val g = op(a, a).reverse(b)

    val x = a.toT0
    val y = b.toT0

    val l = y / x
    val r = (-y * x) / (x * x)

    g(a).get.asInstanceOf[N0].eval[T0] shouldCloseTo l + r // 0.0
  }

  //  FIXME: the differentiation result of (var / var) should be zero
  // but following code doesn't produce it because of a floating-point precision issue.
  property("var0 / var0 reverse node1") = forAll(genV0(), genNonzeroN1(value = denomDomain)) { (a: Var0, b: N1) =>
    val g = op(a, a).reverse(b)

    val x = a.toT0
    val y = b.toT1

    val l = y div x
    val r = (y.neg mul x) div (x * x)

    g(a).get.asInstanceOf[N1].eval[T1] shouldCloseTo (l add r) // zero1(a)
  }

  //  FIXME: the differentiation result of (var / var) should be zero
  // but following code doesn't produce it because of a floating-point precision issue.
  property("var0 / var0 reverse node2") = forAll(genV0(), genNonzeroN2(value = denomDomain)) { (a: Var0, b: N2) =>
    val g = op(a, a).reverse(b)

    val x = a.toT0
    val y = b.toT2

    val l = y div x
    val r = (y.neg mul x) div (x * x)
    g(a).get.asInstanceOf[N2].eval[T2] shouldCloseTo (l add r) // zero2(b)

  }
  */

}


trait Div00Spec extends BinaryOp00SpecBase { self: Properties with SpecBackend =>

  def denomDomain: Gen[T0]

  override def op(a: N0, b: N0): N0 = Div00(a, b)

  override def op(a: String, b: String): String = s"$a / $b"

  override def genRightArgN0ForSpecBase: Gen[N0] = genNonzeroN0(denomDomain)

  override def genRightArgNV0ForSpecBase: Gen[N0] = genNonzeroNV0(denomDomain)

  override def genRightArgV0ForSpecBase: Gen[Var0] = genV0(denomDomain)

}

