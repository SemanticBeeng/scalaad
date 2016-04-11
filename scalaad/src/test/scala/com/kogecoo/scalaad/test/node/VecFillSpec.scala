package com.kogecoo.scalaad.test.node

import com.kogecoo.scalaad.Shape1
import com.kogecoo.scalaad.graph._
import com.kogecoo.scalaad.impl.std.Implicits._
import com.kogecoo.scalaad.test.helper.impl.std.Implicits._
import org.scalacheck.Prop.forAll
import org.scalacheck.Properties


object VecFillSpec extends Properties("VecFill") with NodeSpecBase {

  def op(a: N0, s: S1): N1 = VecFill(a, s)

  def op(argStr: String): String = s"vec($argStr)"


  property("eval") = forAll(genN0(), genS1()) { (a: N0, s: S1) =>
    op(a, s).eval[T1] shouldCloseTo const1(a.toStd, s)
  }

  property(s"${op("node0")} forward w.r.t node0") = forAll(genN0(), genN0(), genS1()) { (a: N0, b: N0, s: S1) =>
    op(a, s).forward[N0, N1](b).eval[T1] shouldCloseTo zero1(s)
  }

  property(s"${op("var0")} forward w.r.t self") = forAll(genV0(), genS1()) { (a: Var0, s: S1) =>
    op(a, s).forward[N0, N1](a).eval[T1] shouldCloseTo one1(s)
  }


  property(s"${op("nonvar0")} reverse node0") = forAll(genNV0(), genN0(), genS1()) { (a: N0, b: N0, s: S1) =>
    op(a, s).reverse(b).size == 0
  }

  property(s"${op("nonvar0")} reverse node1") = forAll(genNV0(), genN1()) { (a: N0, b: N1) =>
    op(a, b.shape).reverse(b).size == 0
  }

  property(s"${op("nonvar0")} reverse node2") = forAll(genNV0(), genN2()) { (a: N0, b: N2) =>
    op(a, Shape1(b.shape._1)).reverse(b).size == 0
  }

  property(s"${op("var0")} reverse node0") = forAll(genV0(), genN0(), genS1()) { (a: Var0, b: N0, s: S1) =>
    val g = op(a, s).reverse(b)
    g(a).get.asInstanceOf[N1].eval[T1] shouldCloseTo (one1(s) mul b.toStd)
  }

  property(s"${op("var0")} reverse node1") = forAll(genV0(), genN1()) { (a: Var0, b: N1) =>
    val g = op(a, b.shape).reverse(b)
    g(a).get.asInstanceOf[N1].eval[T1] shouldCloseTo b.toStd
  }

  property(s"${op("var0")} reverse node2") = forAll(genV0(), genN2()) { (a: Var0, b: N2) =>
    val g = op(a, Shape1(b.shape._1)).reverse(b)
    g(a).get.asInstanceOf[N2].eval[T2] shouldCloseTo b.toStd
  }

}


