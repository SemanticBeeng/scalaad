package com.kogecoo.scalaad.test.node

import com.kogecoo.scalaad.graph._
import com.kogecoo.scalaad.impl.std.Implicits._
import com.kogecoo.scalaad.test.helper.impl.std.Implicits._
import org.scalacheck.Prop.forAll
import org.scalacheck.Properties


object Dot11Spec extends Properties("Dotl11") with NodeSpecBase {

  override val defaultMinValue = Some(-1e100)
  override val defaultMaxValue = Some(1e100)

  def op(a: N1, b: N1): N0 = Dot11(a, b)

  def vec(a: N0, s: S1): N1 = VecFill(a, s)
/*
  property("eval") = forAll(genN1_N1()) { case (a: N1, b: N1) =>
    op(a, b).eval[T0] shouldCloseTo (a.toStd dot b.toStd)
  }

  property("node1 dot node1 forward w.r.t node0") = forAll(genN1_N1(), genN0()) { case ((a: N1, b: N1), c: N0) =>
    op(a, b).forward[N0, N0](c).eval[T0] shouldCloseTo 0.0
  }

  property("vec(var0) dot node1 forward w.r.t left") = forAll(genV0(), genN1()) { case (a: Var0, b: N1) =>
    op(vec(a, b.shape), b).forward[N0, N0](a).eval[T0] shouldCloseTo b.toStd.sum
  }

  property("node1 dot vec(var0) forward w.r.t right") = forAll(genN1(), genV0()) { case (a: N1, b: Var0) =>
    op(a, vec(b, a.shape)).forward[N0, N0](b).eval[T0] shouldCloseTo a.toStd.sum
  }

  property("vec(var0) dot vec(var0) forward w.r.t self") = forAll(genV0(), genS1) { case (a: Var0, s: S1) =>
    op(vec(a, s), vec(a, s)).forward[N0, N0](a).eval[T0] shouldCloseTo vec(a, s).toStd.sum * 2
  }

  property("nonvar1 dot nonvar1 reverse node0") = forAll(genNV1_NV1(), genN0()) { case ((a: N1, b: N1), c: N0) =>
    op(a, b).reverse(c).size == 0
  }
*/
  property("var1 dot nonvar1 reverse node0") = forAll(genV1_NV1(), genN0()) { case ((a: Var1, b: N1), c: N0) =>
    val g = op(a, b).reverse(c)
    g(a).get.asInstanceOf[N1].eval[T1] shouldCloseTo const1(b.toStd.sum * c.toStd, a.shape)
  }

  property("var1 dot nonvar1 reverse node1") = forAll(genV1_NV1_N1()) { case(a: Var1, b: N1, c: N1) =>
    val g = op(a, b).reverse(c)
    g(a).get.asInstanceOf[N1].eval[T1] shouldCloseTo (c.toStd mul b.toStd.sum)
  }
/*
  property("var1 dot nonvar1 reverse node2") = forAll(genV1_NV1_RowEquivN2()) { case (a: Var1, b: N1, c: N2) =>
    val g = op(a, b).reverse(c)
    g(a).get.asInstanceOf[N2].eval[T2] shouldCloseTo (c.toStd colMul b.toStd)
  }

  property("nonvar1 dot var1 reverse node0") = forAll(genNV1_V1(), genN0()) { case ((a: N1, b: Var1), c: N0) =>
    val g = op(a, b).reverse(c)
    g(b).get.asInstanceOf[N1].eval[T1] shouldCloseTo (a.toStd mul c.toStd)
  }

  property("nonvar1 dot var1 reverse node1") = forAll(genNV1_V1_N1()) { case (a: N1, b: Var1, c: N1) =>
    val g = op(a, b).reverse(c)
    g(b).get.asInstanceOf[N1].eval[T1] shouldCloseTo (c.toStd mul a.toStd)
  }

  property("nonvar1 dot var1 reverse node2") = forAll(genNV1_V1_RowEquivN2()) { case (a: N1, b: Var1, c: N2) =>
    val g = op(a, b).reverse(c)
    g(b).get.asInstanceOf[N2].eval[T2] shouldCloseTo (c.toStd colMul a.toStd)
  }

  property("var1 dot var1 reverse node0") = forAll(genV1(), genN0()) { (a: Var1, b: N0) =>
    val g = op(a, a).reverse(b)
    g(a).get.asInstanceOf[N1].eval[T1] shouldCloseTo (a.toStd mul (2 * b.toStd))
  }

  property("var1 dot var1 reverse node1") = forAll(genV1_N1()) { case (a: Var1, b: N1) =>
    val g = op(a, a).reverse(b)
    g(a).get.asInstanceOf[N1].eval[T1] shouldCloseTo (a.toStd mul (b.toStd mul 2))
  }

  property("var1 dot var1 reverse node2") = forAll(genV1_RowEquivN2()) { case (a: Var1, b: N2) =>
    val g = op(a, a).reverse(b)
    g(a).get.asInstanceOf[N2].eval[T2] shouldCloseTo (b.toStd colMul (a.toStd mul 2))
  }
*/
}
