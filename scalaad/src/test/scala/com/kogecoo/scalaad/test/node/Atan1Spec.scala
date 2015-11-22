package com.kogecoo.scalaad.test.node

import com.kogecoo.scalaad.graph._
import com.kogecoo.scalaad.impl.std.Implicits._
import com.kogecoo.scalaad.test.helper.impl.std.Implicits._
import org.scalacheck.Prop.forAll
import org.scalacheck.Properties


object Atan1Spec extends Properties("Atan1") with NodeSpecBase {

  private[this] def deriv(x: Double): Double = 1.0 / (1.0 + x * x)

  override val defaultMinValue = Some(-1e15)
  override val defaultMaxValue = Some( 1e15)

  property("eval") = forAll(genN1()) { (a: N1) =>
    Atan1(a).eval[T1] shouldCloseTo a.toStd.map(math.atan)
  }

  property("atan(node1) forward w.r.t node0") = forAll(genN1(), genN0()) { (a: N1, b: N0) =>
    Atan1(a).forward[N0, N1](b).eval[T1] shouldCloseTo zero1(a)
  }

  property("atan(node1) forward w.r.t node1") = forAll(genN1(), genN1()) { (a: N1, b: N1) =>
    Atan1(a).forward[N1, N2](b).eval[T2] shouldCloseTo zero2(a, b)
  }

  property("atan(var1) forward w.r.t self") = forAll(genV1()) { (a: Var1) =>
    Atan1(a).forward[N1, N2](a).eval[T2] shouldCloseTo diag(a.toStd.map(deriv))
  }


  property("atan(nonvar1) reverse node0") = forAll(genNV1(), genN0()) { (a: N1, b: N0) =>
    Atan1(a).reverse(b).size == 0
  }

  property("atan(nonvar1) reverse node1") = forAll(genNV1_NV1()) { case (a: N1, b: N1) =>
    Atan1(a).reverse(b).size == 0
  }

  property("atan(nonvar1) reverse node2") = forAll(genNV1_RowEquivN2()) { case (a: N1, b: N2) =>
    Atan1(a).reverse(b).size == 0
  }

  property("atan(var1) reverse node0") = forAll(genV1(), genN0()) { (a: Var1, b: N0) =>
    val g = Atan1(a).reverse(b)
    g(a).get.asInstanceOf[N1].eval[T1] shouldCloseTo (a.toStd.map(deriv) mul b.toStd)
  }

  property("atan(var1) reverse node1") = forAll(genV1_N1()) { case (a: Var1, b: N1) =>
    val g = Atan1(a).reverse(b)
    g(a).get.asInstanceOf[N1].eval[T1] shouldCloseTo (a.toStd.map(deriv) mul b.toStd)
  }

  property("atan(var1) reverse node2") = forAll(genV1_RowEquivN2()) { case (a: Var1, b: N2) =>
    val g = Atan1(a).reverse(b)
    g(a).get.asInstanceOf[N2].eval[T2] shouldCloseTo (b.toStd colMul a.toStd.map(deriv))
  }

}
