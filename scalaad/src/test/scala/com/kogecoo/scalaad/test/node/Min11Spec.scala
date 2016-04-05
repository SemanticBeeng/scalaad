package com.kogecoo.scalaad.test.node

import com.kogecoo.scalaad.graph._
import com.kogecoo.scalaad.impl.std.Implicits._
import com.kogecoo.scalaad.test.helper.impl.std.Implicits._
import org.scalacheck.Prop.forAll
import org.scalacheck.Properties


object Min11Spec extends Properties("Min11") with NodeSpecBase {

  def min(x: T0, y: T0): T0 = math.min(x, y)

  def minL0R1(x: T0, y: T0): T0 = if (x < y) 0.0 else 1.0

  def minL1R0(x: T0, y: T0): T0 = if (x < y) 1.0 else 0.0

  property("eval") = forAll(genN1_N1()) { case (a: N1, b: N1) =>
    Min11(a, b).eval[T1] shouldCloseTo a.toStd.elementwise(b.toStd, min)
  }

  property("min(node1, node1) forward w.r.t node0") = forAll(genN1_N1(), genN0()) { case ((a: N1, b: N1), c: N0) =>
    Min11(a, b).forward[N0, N1](c).eval[T1] shouldCloseTo zero1(b)
  }

  property("min(node1, node1) forward w.r.t node1") = forAll(genN1_N1_N1()) { case (a: N1, b: N1, c: N1) =>
    Min11(a, b).forward[N1, N2](c).eval[T2] shouldCloseTo zero2(a, b)
  }

  property("min(var1, node1) forward w.r.t left") = forAll(genV1_N1()) { case (a: Var1, b: N1) =>
    val x = a.toStd
    val y = b.toStd
    Min11(a, b).forward[N1, N2](a).eval[T2] shouldCloseTo diag(x.elementwise(y, minL1R0))
  }

  property("min(node1, var1) forward w.r.t right") = forAll(genN1_V1()) { case (a: N1, b: Var1) =>
    val x = a.toStd
    val y = b.toStd
    Min11(a, b).forward[N1, N2](b).eval[T2] shouldCloseTo diag(x.elementwise(y, minL0R1))
  }

  property("min(var1, var1) forward w.r.t self") = forAll(genV1()) { (a: Var1) =>
    Min11(a, a).forward[N1, N2](a).eval[T2] shouldCloseTo diag(one1(a))
  }
/*
  property("min(nonvar1, nonvar1) reverse node0") = forAll(genNV1_NV1(), genN0()) { case ((a: N1, b: N1), c: N0) =>
    Min11(a, b).reverse(c).size == 0
  }

  property("min(nonvar1, nonvar1) reverse node1") = forAll(genNV1_NV1_N1()) { case (a: N1, b: N1, c: N1) =>
    Min11(a, b).reverse(c).size == 0
  }

  property("min(nonvar1, nonvar1) reverse node2") = forAll(genNV1_NV1_RowEquivN2()) { case (a: N1, b: N1, c: N2) =>
    Min11(a, b).reverse(c).size == 0
  }

  property("min(var1, nonvar1) reverse node0") = forAll(genV1_NV1(), genN0()) { case ((a: Var1, b: N1), c: N0) =>
    val g = Min11(a, b).reverse(c)
    g(a).get.asInstanceOf[N1].eval[T1] shouldCloseTo
  }

  property("min(var1, nonvar1) reverse node1") = forAll(genV1_NV1_N1()) { case(a: Var1, b: N1, c: N1) =>
    val g = Min11(a, b).reverse(c)
    g(a).get.asInstanceOf[N1].eval[T1] shouldCloseTo
  }

  property("min(var1, nonvar1) reverse node2") = forAll(genV1_NV1_RowEquivN2()) { case (a: Var1, b: N1, c: N2) =>
    val g = Min11(a, b).reverse(c)
    g(a).get.asInstanceOf[N2].eval[T2] shouldCloseTo
  }

  property("min(nonvar1, var1) reverse node0") = forAll(genNV1_V1(), genN0()) { case ((a: N1, b: Var1), c: N0) =>
    val g = Min11(a, b).reverse(c)
    g(b).get.asInstanceOf[N1].eval[T1] shouldCloseTo
  }

  property("min(nonvar1, var1) reverse node1") = forAll(genNV1_V1_N1()) { case (a: N1, b: Var1, c: N1) =>
    val g = Min11(a, b).reverse(c)
    g(b).get.asInstanceOf[N1].eval[T1] shouldCloseTo
  }

  property("min(nonvar1, var1) reverse node2") = forAll(genNV1_V1_RowEquivN2()) { case (a: N1, b: Var1, c: N2) =>
    val g = Min11(a, b).reverse(c)
    g(b).get.asInstanceOf[N2].eval[T2] shouldCloseTo
  }

  property("min(var1, var1) reverse node0") = forAll(genV1(), genN0()) { (a: Var1, b: N0) =>
    val g = Min11(a, a).reverse(b)
    g(a).get.asInstanceOf[N1].eval[T1] shouldCloseTo
  }

  property("min(var1, var1) reverse node1") = forAll(genV1_N1()) { case (a: Var1, b: N1) =>
    val g = Min11(a, a).reverse(b)
    g(a).get.asInstanceOf[N1].eval[T1] shouldCloseTo
  }

  property("min(var1, var1) reverse node2") = forAll(genV1_RowEquivN2()) { case (a: Var1, b: N2) =>
    val g = Min11(a, a).reverse(b)
    g(a).get.asInstanceOf[N2].eval[T2] shouldCloseTo
  }
*/
}

