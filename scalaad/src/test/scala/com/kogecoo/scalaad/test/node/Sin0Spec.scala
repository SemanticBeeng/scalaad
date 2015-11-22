package com.kogecoo.scalaad.test.node

import com.kogecoo.scalaad.graph._
import com.kogecoo.scalaad.impl.std.Implicits._
import com.kogecoo.scalaad.test.helper.impl.std.Implicits._
import org.scalacheck.Prop.forAll
import org.scalacheck.Properties


object Sin0Spec extends Properties("Sin0") with NodeSpecBase {

  property("eval") = forAll(genN0()) { (a: N0) =>
    Sin0(a).eval[T0] shouldCloseTo math.sin(a.toStd)
  }

  property("sin(node0) forward w.r.t node0") = forAll(genN0(), genN0()) { (a: N0, b: N0) =>
    Sin0(a).forward[N0, N0](b).eval[T0] shouldCloseTo 0.0
  }

  property("sin(node0) forward w.r.t node1") = forAll(genN0(), genN1()) { (a: N0, b: N1) =>
    Sin0(a).forward[N1, N1](b).eval[T1] shouldCloseTo zero1(b)
  }

  property("sin(node0) forward w.r.t node2") = forAll(genN0(), genN2()) { (a: N0, b: N2) =>
    Sin0(a).forward[N2, N2](b).eval[T2] shouldCloseTo zero2(b)
  }

  property("sin(var0) forward w.r.t self") = forAll(genV0()) { (a: Var0) =>
    Sin0(a).forward[N0, N0](a).eval[T0] shouldCloseTo math.cos(a.toStd)
  }


  property("sin(nonvar0) reverse node0") = forAll(genNV0(), genN0()) { (a: N0, b: N0) =>
    Sin0(a).reverse(b).size == 0
  }

  property("sin(nonvar0) reverse node1") = forAll(genNV0(), genN1()) { (a: N0, b: N1) =>
    Sin0(a).reverse(b).size == 0
  }

  property("sin(nonvar0) reverse node2") = forAll(genNV0(), genN2()) { (a: N0, b: N2) =>
    Sin0(a).reverse(b).size == 0
  }

  property("sin(var0) reverse node0") = forAll(genV0(), genN0()) { (a: Var0, b: N0) =>
    val g = Sin0(a).reverse(b)
    g(a).get.asInstanceOf[N0].eval[T0] shouldCloseTo b.toStd * math.cos(a.toStd)
  }

  property("sin(var0) reverse node1") = forAll(genV0(), genN1()) { (a: Var0, b: N1) =>
    val g = Sin0(a).reverse(b)
    g(a).get.asInstanceOf[N1].eval[T1] shouldCloseTo (b.toStd mul math.cos(a.toStd))
  }

  property("sin(var0) reverse node2") = forAll(genV0(), genN2()) { (a: Var0, b: N2) =>
    val g = Sin0(a).reverse(b)
    g(a).get.asInstanceOf[N2].eval[T2] shouldCloseTo (b.toStd mul math.cos(a.toStd))
  }

}

