package com.kogecoo.scalaad.test.node

import com.kogecoo.scalaad.graph._
import com.kogecoo.scalaad.impl.std.Implicits._
import com.kogecoo.scalaad.test.helper.impl.std.Implicits._
import com.kogecoo.scalaad.{StdMat, StdVec}
import org.scalacheck.Prop.forAll
import org.scalacheck.Properties


object Tanh0Spec extends Properties("Tanh0") with NodeSpecBase {

  def deriv(x: Double): Double = 1 - math.tanh(x) * math.tanh(x)

  property("eval") = forAll(genN0()) { (a: N0) =>
    Tanh0(a).eval[T0] shouldCloseTo math.tanh(a.toStd)
  }

  property("cosh(node0) forward w.r.t node0") = forAll(genN0(), genN0()) { (a: N0, b: N0) =>
    Tanh0(a).forward[N0, N0](b).eval[T0] shouldCloseTo 0.0
  }

  property("cosh(node0) forward w.r.t node1") = forAll(genN0(), genN1()) { (a: N0, b: N1) =>
    Tanh0(a).forward[N1, N1](b).eval[StdVec[T0]] shouldCloseTo zero1(b)
  }

  property("cosh(node0) forward w.r.t node2") = forAll(genN0(), genN2()) { (a: N0, b: N2) =>
    Tanh0(a).forward[N2, N2](b).eval[StdMat[T0]] shouldCloseTo zero2(b)
  }

  property("cosh(var0) forward w.r.t self") = forAll(genV0()) { (a: Var0) =>
    Tanh0(a).forward[N0, N0](a).eval[T0] shouldCloseTo deriv(a.toStd)
  }


  property("cosh(nonvar0) reverse node0") = forAll(genNV0(), genN0()) { (a: N0, b: N0) =>
    Tanh0(a).reverse(b).size == 0
  }

  property("cosh(nonvar0) reverse node1") = forAll(genNV0(), genN1()) { (a: N0, b: N1) =>
    Tanh0(a).reverse(b).size == 0
  }

  property("cosh(nonvar0) reverse node2") = forAll(genNV0(), genN2()) { (a: N0, b: N2) =>
    Tanh0(a).reverse(b).size == 0
  }

  property("cosh(var0) reverse node0") = forAll(genV0(), genN0()) { (a: Var0, b: N0) =>
    val g = Tanh0(a).reverse(b)
    g(a).get.asInstanceOf[N0].eval[T0] shouldCloseTo b.toStd * deriv(a.toStd)
  }

  property("cosh(var0) reverse node1") = forAll(genV0(), genN1()) { (a: Var0, b: N1) =>
    val g = Tanh0(a).reverse(b)
    g(a).get.asInstanceOf[N1].eval[StdVec[T0]] shouldCloseTo (b.toStd mul deriv(a.toStd))
  }

  property("cosh(var0) reverse node2") = forAll(genV0(), genN2()) { (a: Var0, b: N2) =>
    val g = Tanh0(a).reverse(b)
    g(a).get.asInstanceOf[N2].eval[StdMat[T0]] shouldCloseTo (b.toStd mul deriv(a.toStd))
  }

}


