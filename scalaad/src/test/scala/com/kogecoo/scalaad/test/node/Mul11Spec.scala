package com.kogecoo.scalaad.test.node

import com.kogecoo.scalaad.graph._
import com.kogecoo.scalaad.impl.std.Implicits._
import com.kogecoo.scalaad.test.helper.impl.std.Implicits._
import org.scalacheck.Prop.forAll
import org.scalacheck.Properties


object Mul11Spec extends Properties("Mul11") with NodeSpecBase {

  override val defaultMinValue = Some(-1e100)
  override val defaultMaxValue = Some(1e100)

  property("eval") = forAll(genN1_N1()) { case (a: N1, b: N1) =>
    Mul11(a, b).eval[T1] shouldCloseTo (a.toStd mul b.toStd)
  }

  property("node1 * node1 forward w.r.t node0") = forAll(genN1_N1(), genN0()) { case ((a: N1, b: N1), c: N0) =>
    Mul11(a, b).forward[N0, N1](c).eval[T1] shouldCloseTo zero1(b)
  }

  property("node1 * node1 forward w.r.t node1") = forAll(genN1_N1_N1()) { case (a: N1, b: N1, c: N1) =>
    Mul11(a, b).forward[N1, N2](c).eval[T2] shouldCloseTo zero2(a, b)
  }

  property("var1 * node1 forward w.r.t left") = forAll(genV1_N1()) { case (a: Var1, b: N1) =>
    Mul11(a, b).forward[N1, N2](a).eval[T2] shouldCloseTo diag(b.toStd)
  }

  property("node1 * var1 forward w.r.t right") = forAll(genN1_V1()) { case (a: N1, b: Var1) =>
    Mul11(a, b).forward[N1, N2](b).eval[T2] shouldCloseTo diag(a.toStd)
  }

  property("var1 * var1 forward w.r.t self") = forAll(genV1()) { (a: Var1) =>
    Mul11(a, a).forward[N1, N2](a).eval[T2] shouldCloseTo diag(a.toStd mul 2)
  }

  property("nonvar1 * nonvar1 reverse node0") = forAll(genNV1_NV1(), genN0()) { case ((a: N1, b: N1), c: N0) =>
    Mul11(a, b).reverse(c).size == 0
  }

  property("nonvar1 * nonvar1 reverse node1") = forAll(genNV1_NV1_N1()) { case (a: N1, b: N1, c: N1) =>
    Mul11(a, b).reverse(c).size == 0
  }

  property("nonvar1 * nonvar1 reverse node2") = forAll(genNV1_NV1_RowEquivN2()) { case (a: N1, b: N1, c: N2) =>
    Mul11(a, b).reverse(c).size == 0
  }

  property("var1 * nonvar1 reverse node0") = forAll(genV1_NV1(), genN0()) { case ((a: Var1, b: N1), c: N0) =>
    val g = Mul11(a, b).reverse(c)
    g(a).get.asInstanceOf[N1].eval[T1] shouldCloseTo (b.toStd mul c.toStd)
  }

  property("var1 * nonvar1 reverse node1") = forAll(genV1_NV1_N1()) { case(a: Var1, b: N1, c: N1) =>
    val g = Mul11(a, b).reverse(c)
    g(a).get.asInstanceOf[N1].eval[T1] shouldCloseTo (b.toStd mul c.toStd)
  }

  property("var1 * nonvar1 reverse node2") = forAll(genV1_NV1_RowEquivN2()) { case (a: Var1, b: N1, c: N2) =>
    val g = Mul11(a, b).reverse(c)
    g(a).get.asInstanceOf[N2].eval[T2] shouldCloseTo (c.toStd colMul b.toStd)
  }

  property("nonvar1 * var1 reverse node0") = forAll(genNV1_V1(), genN0()) { case ((a: N1, b: Var1), c: N0) =>
    val g = Mul11(a, b).reverse(c)
    g(b).get.asInstanceOf[N1].eval[T1] shouldCloseTo (a.toStd mul c.toStd)
  }

  property("nonvar1 * var1 reverse node1") = forAll(genNV1_V1_N1()) { case (a: N1, b: Var1, c: N1) =>
    val g = Mul11(a, b).reverse(c)
    g(b).get.asInstanceOf[N1].eval[T1] shouldCloseTo (c.toStd mul a.toStd)
  }

  property("nonvar1 * var1 reverse node2") = forAll(genNV1_V1_RowEquivN2()) { case (a: N1, b: Var1, c: N2) =>
    val g = Mul11(a, b).reverse(c)
    g(b).get.asInstanceOf[N2].eval[T2] shouldCloseTo (c.toStd colMul a.toStd)
  }

  property("var1 * var1 reverse node0") = forAll(genV1(), genN0()) { (a: Var1, b: N0) =>
    val g = Mul11(a, a).reverse(b)
    g(a).get.asInstanceOf[N1].eval[T1] shouldCloseTo (a.toStd mul (2 * b.toStd))
  }

  property("var1 * var1 reverse node1") = forAll(genV1_N1()) { case (a: Var1, b: N1) =>
    val g = Mul11(a, a).reverse(b)
    g(a).get.asInstanceOf[N1].eval[T1] shouldCloseTo (a.toStd mul (b.toStd mul 2))
  }

  property("var1 * var1 reverse node2") = forAll(genV1_RowEquivN2()) { case (a: Var1, b: N2) =>
    val g = Mul11(a, a).reverse(b)
    g(a).get.asInstanceOf[N2].eval[T2] shouldCloseTo (b.toStd colMul (a.toStd mul 2))
  }

}

