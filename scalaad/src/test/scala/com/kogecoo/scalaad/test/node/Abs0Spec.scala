package com.kogecoo.scalaad.test.node

import com.kogecoo.scalaad.graph._
import com.kogecoo.scalaad.impl.std.Implicits._
import com.kogecoo.scalaad.test.helper.impl.std.Implicits._
import org.scalacheck.Prop.forAll
import org.scalacheck.Properties


object Abs0Spec extends Properties("Abs0") with NodeSpecBase {

  private[this] def deriv(x: Double, y: Double): Double = if (x > 0) y else -y


  property("eval") = forAll(genN0()) { (a: N0) =>
    val x = a.toStd
    Abs0(a).eval[T0] shouldCloseTo math.abs(x)
  }

  property("abs(node0)) forward w.r.t node0") = forAll(genN0(), genN0()) { (a: N0, b: N0) =>
    Abs0(a).forward[N0, N0](b).eval[T0] shouldCloseTo 0.0
  }

  property("abs(node0)) forward w.r.t node1") = forAll(genN0(), genN1()) { (a: N0, b: N1) =>
    Abs0(a).forward[N1, N1](b).eval[T1] shouldCloseTo zero1(b)
  }

  property("abs(node0)) forward w.r.t node2") = forAll(genN0(), genN2()) { (a: N0, b: N2) =>
    Abs0(a).forward[N2, N2](b).eval[T2] shouldCloseTo zero2(b)
  }

  property("abs(var0)) forward w.r.t self") = forAll(genV0()) { (a: Var0) =>
    Abs0(a).forward[N0, N0](a).eval[T0] shouldCloseTo deriv(a.toStd, 1.0)
  }

/*
  property("abs(nonvar0)) reverse node0") = forAll(genNV0(), genN0()) { (a: N0, b: N0) =>
    Abs0(a).reverse(b).size == 0
  }

  property("abs(nonvar0)) reverse node1") = forAll(genNV0(), genN1()) { (a: N0, b: N1) =>
    Abs0(a).reverse(b).size == 0
  }

  property("abs(nonvar0)) reverse node2") = forAll(genNV0(), genN2()) { (a: N0, b: N2) =>
    Abs0(a).reverse(b).size == 0
  }

  property("abs(var0)) reverse node0") = forAll(genV0(), genN0()) { (a: Var0, b: N0) =>
    val g = Abs0(a).reverse(b)
    g(a).get.asInstanceOf[N0].eval[T0] shouldCloseTo
  }

  property("abs(var0)) reverse node1") = forAll(genV0(), genN1()) { (a: Var0, b: N1) =>
    val g = Abs0(a).reverse(b)
    g(a).get.asInstanceOf[N1].eval[T1] shouldCloseTo
  }

  property("abs(var0)) reverse node2") = forAll(genV0(), genN2()) { (a: Var0, b: N2) =>
    val g = Abs0(a).reverse(b)
    g(a).get.asInstanceOf[N2].eval[T2] shouldCloseTo
  }
*/
}


