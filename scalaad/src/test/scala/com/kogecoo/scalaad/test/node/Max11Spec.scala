package com.kogecoo.scalaad.test.node

import com.kogecoo.scalaad.graph._
import com.kogecoo.scalaad.impl.std.Implicits._
import com.kogecoo.scalaad.test.helper.impl.std.Implicits._
import org.scalacheck.Prop.forAll
import org.scalacheck.Properties


object Max11Spec extends Properties("Max11") with NodeSpecBase {

  def max(x: T0, y: T0): T0 = math.max(x, y)

  def maxL0R1(x: T0, y: T0): T0 = if (x > y) 0.0 else 1.0

  def maxL1R0(x: T0, y: T0): T0 = if (x > y) 1.0 else 0.0

  property("eval") = forAll(genN1_N1()) { case (a: N1, b: N1) =>
    Max11(a, b).eval[T1] shouldCloseTo a.toStd.elementwise(b.toStd, max)
  }

  property("max(node1, node1) forward w.r.t node0") = forAll(genN1_N1(), genN0()) { case ((a: N1, b: N1), c: N0) =>
    Max11(a, b).forward[N0, N1](c).eval[T1] shouldCloseTo zero1(b)
  }

  property("max(node1, node1) forward w.r.t node1") = forAll(genN1_N1_N1()) { case (a: N1, b: N1, c: N1) =>
    Max11(a, b).forward[N1, N2](c).eval[T2] shouldCloseTo zero2(a, b)
  }

  property("max(var1, node1) forward w.r.t left") = forAll(genV1_N1()) { case (a: Var1, b: N1) =>
    val x = a.toStd
    val y = b.toStd
    Max11(a, b).forward[N1, N2](a).eval[T2] shouldCloseTo diag(x.elementwise(y, maxL1R0))
  }

  property("max(node1, var1) forward w.r.t right") = forAll(genN1_V1()) { case (a: N1, b: Var1) =>
    val x = a.toStd
    val y = b.toStd
    Max11(a, b).forward[N1, N2](b).eval[T2] shouldCloseTo diag(x.elementwise(y, maxL0R1))
  }

  property("max(var1, var1) forward w.r.t self") = forAll(genV1()) { (a: Var1) =>
    Max11(a, a).forward[N1, N2](a).eval[T2] shouldCloseTo diag(one1(a))
  }
/*
  property("max(nonvar1, nonvar1) reverse node0") = forAll(genNV1_NV1(), genN0()) { case ((a: N1, b: N1), c: N0) =>
    Max11(a, b).reverse(c).size == 0
  }

  property("max(nonvar1, nonvar1) reverse node1") = forAll(genNV1_NV1_N1()) { case (a: N1, b: N1, c: N1) =>
    Max11(a, b).reverse(c).size == 0
  }

  property("max(nonvar1, nonvar1) reverse node2") = forAll(genNV1_NV1_RowEquivN2()) { case (a: N1, b: N1, c: N2) =>
    Max11(a, b).reverse(c).size == 0
  }

  property("max(var1, nonvar1) reverse node0") = forAll(genV1_NV1(), genN0()) { case ((a: Var1, b: N1), c: N0) =>
    val g = Max11(a, b).reverse(c)
    g(a).get.asInstanceOf[N1].eval[T1] shouldCloseTo
  }

  property("max(var1, nonvar1) reverse node1") = forAll(genV1_NV1_N1()) { case(a: Var1, b: N1, c: N1) =>
    val g = Max11(a, b).reverse(c)
    g(a).get.asInstanceOf[N1].eval[T1] shouldCloseTo
  }

  property("max(var1, nonvar1) reverse node2") = forAll(genV1_NV1_RowEquivN2()) { case (a: Var1, b: N1, c: N2) =>
    val g = Max11(a, b).reverse(c)
    g(a).get.asInstanceOf[N2].eval[T2] shouldCloseTo
  }

  property("max(nonvar1, var1) reverse node0") = forAll(genNV1_V1(), genN0()) { case ((a: N1, b: Var1), c: N0) =>
    val g = Max11(a, b).reverse(c)
    g(b).get.asInstanceOf[N1].eval[T1] shouldCloseTo
  }

  property("max(nonvar1, var1) reverse node1") = forAll(genNV1_V1_N1()) { case (a: N1, b: Var1, c: N1) =>
    val g = Max11(a, b).reverse(c)
    g(b).get.asInstanceOf[N1].eval[T1] shouldCloseTo
  }

  property("max(nonvar1, var1) reverse node2") = forAll(genNV1_V1_RowEquivN2()) { case (a: N1, b: Var1, c: N2) =>
    val g = Max11(a, b).reverse(c)
    g(b).get.asInstanceOf[N2].eval[T2] shouldCloseTo
  }

  property("max(var1, var1) reverse node0") = forAll(genV1(), genN0()) { (a: Var1, b: N0) =>
    val g = Max11(a, a).reverse(b)
    g(a).get.asInstanceOf[N1].eval[T1] shouldCloseTo
  }

  property("max(var1, var1) reverse node1") = forAll(genV1_N1()) { case (a: Var1, b: N1) =>
    val g = Max11(a, a).reverse(b)
    g(a).get.asInstanceOf[N1].eval[T1] shouldCloseTo
  }

  property("max(var1, var1) reverse node2") = forAll(genV1_RowEquivN2()) { case (a: Var1, b: N2) =>
    val g = Max11(a, a).reverse(b)
    g(a).get.asInstanceOf[N2].eval[T2] shouldCloseTo
  }
*/
}

