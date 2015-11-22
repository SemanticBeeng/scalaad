package com.kogecoo.scalaad.test.node

import com.kogecoo.scalaad.graph._
import com.kogecoo.scalaad.impl.std.Implicits._
import com.kogecoo.scalaad.test.helper.impl.std.Implicits._
import com.kogecoo.scalaad.test.helper.impl.std.StdValueGen
import org.scalacheck.Prop.forAll
import org.scalacheck.Properties


object Pow00Spec extends Properties("Pow00") with NodeSpecBase {

  override val defaultMinValue = Some(-100.0)
  override val defaultMaxValue = Some(100.0)

  val expDomain = StdValueGen(
    defaultMinValue,
    defaultMaxValue,
    (x: Double) => true
  )
  val positiveBaseDomain = StdValueGen(
    Some(0.0),
    defaultMaxValue,
    (x: Double) => x != -0.0 && x != 0.0
  )

  def op(a: N0, b: N0): N0 = Pow00(a, b)

  property("eval") = forAll(genNonzeroN0(positiveBaseDomain), genN0(expDomain)) { (a: N0, b: N0) =>
    op(a, b).eval[T0] shouldCloseTo math.pow(a.toStd, b.toStd)
  }

  property("pow(node0, node0) forward w.r.t node0") = forAll(genNonzeroN0(positiveBaseDomain), genN0(expDomain), genN0()) { (a: N0, b: N0, c: N0) =>
    op(a, b).forward[N0, N0](c).eval[T0] shouldCloseTo 0.0
  }

  property("pow(node0, node0) forward w.r.t node1") = forAll(genNonzeroN0(positiveBaseDomain), genN0(expDomain), genN1()) { (a: N0, b: N0, c: N1) =>
    op(a, b).forward[N1, N1](c).eval[T1] shouldCloseTo zero1(c)
  }

  property("pow(node0, node0) forward w.r.t node2") = forAll(genNonzeroN0(positiveBaseDomain), genN0(expDomain), genN2()) { (a: N0, b: N0, c: N2) =>
    op(a, b).forward[N2, N2](c).eval[T2] shouldCloseTo zero2(c)
  }

  property("pow(var0, node0) forward w.r.t left") = forAll(genV0(positiveBaseDomain), genN0(expDomain)) { (a: Var0, b: N0) =>
    val x = a.toStd
    val y = b.toStd
    op(a, b).forward[N0, N0](a).eval[T0] shouldCloseTo y * math.pow(x, y - 1.0)
  }

  property("pow(node0, var0) forward w.r.t right") = forAll(genNonzeroN0(positiveBaseDomain), genV0(expDomain)) { (a: N0, b: Var0) =>
    val x = a.toStd
    val y = b.toStd
    op(a, b).forward[N0, N0](b).eval[T0] shouldCloseTo math.log(x) * math.pow(x, y)
  }

  property("pow(var0, var0) forward w.r.t self") = forAll(genV0(positiveBaseDomain)) { (a: Var0) =>
    val x = a.toStd
    op(a, a).forward[N0, N0](a).eval[T0] shouldCloseTo math.log(x) * math.pow(x, x) + x * math.pow(x, x - 1.0)
  }


  property("pow(nonvar0, nonvar0) reverse node0") = forAll(genNonzeroNV0(positiveBaseDomain), genNV0(expDomain), genN0()) { (a: N0, b: N0, c: N0) =>
    op(a, b).reverse(c).size == 0
  }

  property("pow(nonvar0, nonvar0) reverse node1") = forAll(genNonzeroNV0(positiveBaseDomain), genNV0(expDomain), genN1()) { (a: N0, b: N0, c: N1) =>
    op(a, b).reverse(c).size == 0
  }

  property("pow(nonvar0, nonvar0) reverse node2") = forAll(genNonzeroNV0(positiveBaseDomain), genNV0(expDomain), genN2()) { (a: N0, b: N0, c: N2) =>
    op(a, b).reverse(c).size == 0
  }

  property("pow(var0, nonvar0) reverse node0") = forAll(genV0(positiveBaseDomain), genNV0(expDomain), genN0()) { (a: Var0, b: N0, c: N0) =>
    val x = a.toStd
    val y = b.toStd
    val z = c.toStd
    val g = op(a, b).reverse(c)
    g(a).get.asInstanceOf[N0].eval[T0] shouldCloseTo z * y * math.pow(x, y - 1.0)
  }

  property("pow(var0, nonvar0) reverse node1") = forAll(genV0(positiveBaseDomain), genNV0(expDomain), genN1()) { (a: Var0, b: N0, c: N1) =>
    val x = a.toStd
    val y = b.toStd
    val z = c.toStd
    val g = op(a, b).reverse(c)
    g(a).get.asInstanceOf[N1].eval[T1] shouldCloseTo (z mul (y * math.pow(x, y - 1.0)))
  }

  property("pow(var0, nonvar0) reverse node2") = forAll(genV0(positiveBaseDomain), genNV0(expDomain), genN2()) { (a: Var0, b: N0, c: N2) =>
    val x = a.toStd
    val y = b.toStd
    val z = c.toStd
    val g = op(a, b).reverse(c)
    g(a).get.asInstanceOf[N2].eval[T2] shouldCloseTo (z mul (y * math.pow(x, y - 1.0)))
  }

  property("pow(nonvar0, var0) reverse node0") = forAll(genNonzeroNV0(positiveBaseDomain), genV0(expDomain), genN0()) { (a: N0, b: Var0, c: N0) =>
    val x = a.toStd
    val y = b.toStd
    val z = c.toStd
    val g = op(a, b).reverse(c)
    g(b).get.asInstanceOf[N0].eval[T0] shouldCloseTo z * (math.log(x) * math.pow(x, y))
  }

  property("pow(nonvar0, var0) reverse node1") = forAll(genNonzeroNV0(positiveBaseDomain), genV0(expDomain), genN1()) { (a: N0, b: Var0, c: N1) =>
    val x = a.toStd
    val y = b.toStd
    val z = c.toStd
    val g = op(a, b).reverse(c)
    g(b).get.asInstanceOf[N1].eval[T1] shouldCloseTo (z mul (math.log(x) * math.pow(x, y)))
  }

  property("pow(nonvar0, var0) reverse node2") = forAll(genNonzeroNV0(positiveBaseDomain), genV0(expDomain), genN2()) { (a: N0, b: Var0, c: N2) =>
    val x = a.toStd
    val y = b.toStd
    val z = c.toStd
    val g = op(a, b).reverse(c)
    g(b).get.asInstanceOf[N2].eval[T2] shouldCloseTo (z mul (math.log(x) * math.pow(x, y)))
  }

  property("pow(var0, var0) reverse node0") = forAll(genV0(positiveBaseDomain), genN0(expDomain)) { (a: Var0, b: N0) =>
    val x = a.toStd
    val y = b.toStd

    val l = y * (x * math.pow(x, x - 1.0))
    val r = y * (math.log(x) * math.pow(x, x))

    val g = op(a, a).reverse(b)
    g(a).get.asInstanceOf[N0].eval[T0] shouldCloseTo (l + r)
  }

  property("pow(var0, var0) reverse node1") = forAll(genV0(positiveBaseDomain), genN1(value = expDomain)) { (a: Var0, b: N1) =>
    val x = a.toStd
    val y = b.toStd

    val l = y mul (x * math.pow(x, x - 1.0))
    val r = y mul (math.log(x) * math.pow(x, x))

    val g = op(a, a).reverse(b)
    g(a).get.asInstanceOf[N1].eval[T1] shouldCloseTo (l add r)
  }

  property("pow(var0, var0) reverse node2") = forAll(genV0(positiveBaseDomain), genN2(value = expDomain)) { (a: Var0, b: N2) =>
    val x = a.toStd
    val y = b.toStd

    val l = y mul (x * math.pow(x, x - 1.0))
    val r = y mul (math.log(x) * math.pow(x, x))

    val g = op(a, a).reverse(b)
    g(a).get.asInstanceOf[N2].eval[T2] shouldCloseTo (l add r)
  }

}

