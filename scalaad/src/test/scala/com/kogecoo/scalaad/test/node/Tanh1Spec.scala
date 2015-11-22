package com.kogecoo.scalaad.test.node

import com.kogecoo.scalaad.graph._
import com.kogecoo.scalaad.impl.std.Implicits._
import com.kogecoo.scalaad.test.helper.impl.std.Implicits._
import org.scalacheck.Prop.forAll
import org.scalacheck.Properties


object Tanh1Spec extends Properties("Tanh1") with NodeSpecBase {

  def deriv(x: Double): Double = 1 - math.tanh(x) * math.tanh(x)

  property("eval") = forAll(genN1()) { (a: N1) =>
    Tanh1(a).eval[T1] shouldCloseTo a.toStd.map(math.tanh)
  }

  property("tanh(node1) forward w.r.t node0") = forAll(genN1(), genN0()) { (a: N1, b: N0) =>
    Tanh1(a).forward[N0, N1](b).eval[T1] shouldCloseTo zero1(a)
  }

  property("tanh(node1) forward w.r.t node1") = forAll(genN1(), genN1()) { (a: N1, b: N1) =>
    Tanh1(a).forward[N1, N2](b).eval[T2] shouldCloseTo zero2(a, b)
  }

  property("tanh(var1) forward w.r.t self") = forAll(genV1()) { (a: Var1) =>
    Tanh1(a).forward[N1, N2](a).eval[T2] shouldCloseTo diag(a.toStd.map(deriv))
  }


  property("tanh(nonvar1) reverse node0") = forAll(genNV1(), genN0()) { (a: N1, b: N0) =>
    Tanh1(a).reverse(b).size == 0
  }

  property("tanh(nonvar1) reverse node1") = forAll(genNV1_N1()) { case (a: N1, b: N1) =>
    Tanh1(a).reverse(b).size == 0
  }

  property("tanh(nonvar1) reverse node2") = forAll(genNV1_RowEquivN2()) { case (a: N1, b: N2) =>
    Tanh1(a).reverse(b).size == 0
  }

  property("tanh(var1) reverse node0") = forAll(genV1(), genN0()) { (a: Var1, b: N0) =>
    val g = Tanh1(a).reverse(b)
    g(a).get.asInstanceOf[N1].eval[T1] shouldCloseTo (a.toStd.map(deriv) mul b.toStd)
  }

  property("tanh(var1) reverse node1") = forAll(genV1_N1()) { case (a: Var1, b: N1) =>
    val g = Tanh1(a).reverse(b)
    g(a).get.asInstanceOf[N1].eval[T1] shouldCloseTo (a.toStd.map(deriv) mul b.toStd)
  }

  property("tanh(var1) reverse node2") = forAll(genV1_RowEquivN2()) { case (a: Var1, b: N2) =>
    val g = Tanh1(a).reverse(b)
    g(a).get.asInstanceOf[N2].eval[T2] shouldCloseTo (b.toStd colMul a.toStd.map(deriv))
  }

}


