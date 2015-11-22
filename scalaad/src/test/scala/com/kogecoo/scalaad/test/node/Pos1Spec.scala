package com.kogecoo.scalaad.test.node

import com.kogecoo.scalaad.graph._
import com.kogecoo.scalaad.impl.std.Implicits._
import com.kogecoo.scalaad.test.helper.impl.std.Implicits._
import org.scalacheck.Prop.forAll
import org.scalacheck.Properties


object Pos1Spec extends Properties("Pos1") with NodeSpecBase {

  property("eval") = forAll(genN1()) { (a: N1) =>
    Pos1(a).eval[T1] shouldCloseTo a.toStd
  }

  property("+node1 forward w.r.t node0") = forAll(genN1(), genN0()) { (a: N1, b: N0) =>
    Pos1(a).forward[N0, N1](b).eval[T1] shouldCloseTo zero1(a)
  }

  property("+node1 forward w.r.t node1") = forAll(genN1(), genN1()) { (a: N1, b: N1) =>
    Pos1(a).forward[N1, N2](b).eval[T2] shouldCloseTo zero2(a, b)
  }

  property("+var1 forward w.r.t self") = forAll(genV1()) { (a: Var1) =>
    Pos1(a).forward[N1, N2](a).eval[T2] shouldCloseTo diag(one1(a))
  }


  property("+nonvar1 reverse node0") = forAll(genNV1(), genN0()) { (a: N1, b: N0) =>
    Pos1(a).reverse(b).size == 0
  }

  property("+nonvar1 reverse node1") = forAll(genNV1_N1()) { case (a: N1, b: N1) =>
    Pos1(a).reverse(b).size == 0
  }

  property("+nonvar1 reverse node2") = forAll(genNV1_RowEquivN2()) { case (a: N1, b: N2) =>
    Pos1(a).reverse(b).size == 0
  }

  property("+var1 reverse node0") = forAll(genV1(), genN0()) { (a: Var1, b: N0) =>
    val g = Pos1(a).reverse(b)
    g(a).get.asInstanceOf[N1].eval[T1] shouldCloseTo (one1(a) mul b.toStd)
  }

  property("+var1 reverse node1") = forAll(genV1_N1()) { case (a: Var1, b: N1) =>
    val g = Pos1(a).reverse(b)
    g(a).get.asInstanceOf[N1].eval[T1] shouldCloseTo b.toStd
  }

  property("+var1 reverse node2") = forAll(genV1_RowEquivN2()) { case (a: Var1, b: N2) =>
    val g = Pos1(a).reverse(b)
    g(a).get.asInstanceOf[N2].eval[T2] shouldCloseTo b.toStd
  }

}


