package com.kogecoo.scalaad.test.node

import com.kogecoo.scalaad.graph._
import com.kogecoo.scalaad.impl.std.Implicits._
import com.kogecoo.scalaad.test.helper.impl.std.Implicits._
import org.scalacheck.Prop.forAll
import org.scalacheck.Properties


object Max00Spec extends Properties("Max00") with NodeSpecBase {

  property("eval") = forAll(genN0(), genN0()) { (a: N0, b: N0) =>
    Max00(a, b).eval[T0] shouldCloseTo math.max(a.toStd, b.toStd)
  }

  property("max(node0, node0) forward w.r.t node0") = forAll(genN0(), genN0(), genN0()) { (a: N0, b: N0, c: N0) =>
    Max00(a, b).forward[N0, N0](c).eval[T0] shouldCloseTo 0.0
  }

  property("max(node0, node0) forward w.r.t node1") = forAll(genN0(), genN0(), genN1()) { (a: N0, b: N0, c: N1) =>
    Max00(a, b).forward[N1, N1](c).eval[T1] shouldCloseTo zero1(c)
  }

  property("max(node0, node0) forward w.r.t node2") = forAll(genN0(), genN0(), genN2()) { (a: N0, b: N0, c: N2) =>
    Max00(a, b).forward[N2, N2](c).eval[T2] shouldCloseTo zero2(c)
  }

  property("max(var0, node0) forward w.r.t left") = forAll(genV0(), genN0()) { (a: Var0, b: N0) =>
    val x = a.toStd
    val y = b.toStd
    Max00(a, b).forward[N0, N0](a).eval[T0] shouldCloseTo (if (x > y) 1.0 else 0.0)
  }

  property("max(node0, var0) forward w.r.t right") = forAll(genN0(), genV0()) { (a: N0, b: Var0) =>
    val x = a.toStd
    val y = b.toStd
    Max00(a, b).forward[N0, N0](b).eval[T0] shouldCloseTo (if (x > y) 0.0 else 1.0)
  }

  property("max(var0, var0) forward w.r.t self") = forAll(genV0()) { (a: Var0) =>
    Max00(a, a).forward[N0, N0](a).eval[T0] shouldCloseTo 1.0
  }

/*
  property("max(nonvar0, nonvar0) reverse node0") = forAll(genNV0(), genNV0(), genN0()) { (a: N0, b: N0, c: N0) =>
    Max00(a, b).reverse(c).size == 0
  }

  property("max(nonvar0, nonvar0) reverse node1") = forAll(genNV0(), genNV0(), genN1()) { (a: N0, b: N0, c: N1) =>
    Max00(a, b).reverse(c).size == 0
  }

  property("max(nonvar0, nonvar0) reverse node2") = forAll(genNV0(), genNV0(), genN2()) { (a: N0, b: N0, c: N2) =>
    Max00(a, b).reverse(c).size == 0
  }

  property("max(var0, nonvar0) reverse node0") = forAll(genV0(), genNV0(), genN0()) { (a: Var0, b: N0, c: N0) =>
    val g = Max00(a, b).reverse(c)
    g(a).get.asInstanceOf[N0].eval[T0] shouldCloseTo
  }

  property("max(var0, nonvar0) reverse node1") = forAll(genV0(), genNV0(), genN1()) { (a: Var0, b: N0, c: N1) =>
    val g = Max00(a, b).reverse(c)
    g(a).get.asInstanceOf[N1].eval[T1] shouldCloseTo
  }

  property("max(var0, nonvar0) reverse node2") = forAll(genV0(), genNV0(), genN2()) { (a: Var0, b: N0, c: N2) =>
    val g = Max00(a, b).reverse(c)
    g(a).get.asInstanceOf[N2].eval[T2] shouldCloseTo
  }

  property("max(nonvar0, var0) reverse node0") = forAll(genNV0(), genV0(), genN0()) { (a: N0, b: Var0, c: N0) =>
    val g = Max00(a, b).reverse(c)
    g(b).get.asInstanceOf[N0].eval[T0] shouldCloseTo
  }

  property("max(nonvar0, var0) reverse node1") = forAll(genNV0(), genV0(), genN1()) { (a: N0, b: Var0, c: N1) =>
    val g = Max00(a, b).reverse(c)
    g(b).get.asInstanceOf[N1].eval[T1] shouldCloseTo
  }

  property("max(nonvar0, var0) reverse node2") = forAll(genNV0(), genV0(), genN2()) { (a: N0, b: Var0, c: N2) =>
    val g = Max00(a, b).reverse(c)
    g(b).get.asInstanceOf[N2].eval[T2] shouldCloseTo
  }

  property("max(var0, var0) reverse node0") = forAll(genV0(), genN0()) { (a: Var0, b: N0) =>
    val g = Max00(a, a).reverse(b)
    g(a).get.asInstanceOf[N0].eval[T0] shouldCloseTo
  }

  property("max(var0, var0) reverse node1") = forAll(genV0(), genN1()) { (a: Var0, b: N1) =>
    val g = Max00(a, a).reverse(b)
    g(a).get.asInstanceOf[N1].eval[T1] shouldCloseTo
  }

  property("max(var0, var0) reverse node2") = forAll(genV0(), genN2()) { (a: Var0, b: N2) =>
    val g = Max00(a, a).reverse(b)
    g(a).get.asInstanceOf[N2].eval[T2] shouldCloseTo
  }
*/
}


