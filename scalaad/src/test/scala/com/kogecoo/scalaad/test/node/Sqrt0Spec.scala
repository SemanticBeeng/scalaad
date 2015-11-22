package com.kogecoo.scalaad.test.node

import com.kogecoo.scalaad.graph._
import com.kogecoo.scalaad.impl.std.Implicits._
import com.kogecoo.scalaad.test.helper.impl.std.Implicits._
import org.scalacheck.Prop.forAll
import org.scalacheck.Properties


object Sqrt0Spec extends Properties("Sqrt0") with NodeSpecBase {

  override val defaultMinValue = Some(0.0)
  override val defaultMaxValue = Some(100.0)
  override val defaultValueConstraint = (x: Double) => x != -0.0 && x != 0.0

  property("eval") = forAll(genNonzeroN0()) { (a: N0) =>
    Sqrt0(a).eval[T0] shouldCloseTo math.sqrt(a.toStd)
  }

  property("sqrt(node0) forward w.r.t node0") = forAll(genNonzeroN0(), genN0()) { (a: N0, b: N0) =>
    Sqrt0(a).forward[N0, N0](b).eval[T0] shouldCloseTo 0.0
  }

  property("sqrt(node0) forward w.r.t node1") = forAll(genNonzeroN0(), genN1()) { (a: N0, b: N1) =>
    Sqrt0(a).forward[N1, N1](b).eval[T1] shouldCloseTo zero1(b)
  }

  property("sqrt(node0) forward w.r.t node2") = forAll(genNonzeroN0(), genN2()) { (a: N0, b: N2) =>
    Sqrt0(a).forward[N2, N2](b).eval[T2] shouldCloseTo zero2(b)
  }

  property("sqrt(var0) forward w.r.t self") = forAll(genV0()) { (a: Var0) =>
    Sqrt0(a).forward[N0, N0](a).eval[T0] shouldCloseTo 1 / (2 * math.sqrt(a.toStd))
  }


  property("sqrt(nonvar0) reverse node0") = forAll(genNonzeroNV0(), genN0()) { (a: N0, b: N0) =>
    Sqrt0(a).reverse(b).size == 0
  }

  property("sqrt(nonvar0) reverse node1") = forAll(genNonzeroNV0(), genN1()) { (a: N0, b: N1) =>
    Sqrt0(a).reverse(b).size == 0
  }

  property("sqrt(nonvar0) reverse node2") = forAll(genNonzeroNV0(), genN2()) { (a: N0, b: N2) =>
    Sqrt0(a).reverse(b).size == 0
  }

  property("sqrt(var0) reverse node0") = forAll(genV0(), genN0()) { (a: Var0, b: N0) =>
    val g = Sqrt0(a).reverse(b)
    g(a).get.asInstanceOf[N0].eval[T0] shouldCloseTo b.toStd * (1.0 / (2 * math.sqrt(a.toStd)))
  }

  property("sqrt(var0) reverse node1") = forAll(genV0(), genN1()) { (a: Var0, b: N1) =>
    val g = Sqrt0(a).reverse(b)
    g(a).get.asInstanceOf[N1].eval[T1] shouldCloseTo (b.toStd mul (1.0 / (2 * math.sqrt(a.toStd))))
  }

  property("sqrt(var0) reverse node2") = forAll(genV0(), genN2()) { (a: Var0, b: N2) =>
    val g = Sqrt0(a).reverse(b)
    g(a).get.asInstanceOf[N2].eval[T2] shouldCloseTo (b.toStd mul (1.0 / (2 * math.sqrt(a.toStd))))
  }

}


