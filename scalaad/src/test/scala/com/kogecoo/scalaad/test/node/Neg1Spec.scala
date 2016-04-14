package com.kogecoo.scalaad.test.node

import com.kogecoo.scalaad.graph._
import com.kogecoo.scalaad.impl.std.Implicits._
import com.kogecoo.scalaad.test.helper.impl.std.Implicits._
import org.scalacheck.Prop.forAll
import org.scalacheck.Properties


object Neg1Spec extends Properties("Neg1") with UnaryOp1SpecBase {

  override def op(a: N1): N1 = Neg1(a)

  override def op(argStr: String): String = s"-$argStr"

  property("eval") = forAll(genN1()) { (a: N1) =>
    op(a).eval[T1] shouldCloseTo a.toStd.neg
  }

  property(s"${op("var1")} forward w.r.t self") = forAll(genV1()) { (a: Var1) =>
    op(a).forward[N1, N2](a).eval[T2] shouldCloseTo diag(one1(a).neg)
  }

  property(s"${op("var1")} reverse node0") = forAll(genV1(), genN0()) { (a: Var1, b: N0) =>
    val g = op(a).reverse(b)
    g(a).get.asInstanceOf[N1].eval[T1] shouldCloseTo (one1(a).neg mul b.toStd)
  }

  property(s"${op("var1")} reverse node1") = forAll(genV1_N1()) { case (a: Var1, b: N1) =>
    val g = op(a).reverse(b)
    g(a).get.asInstanceOf[N1].eval[T1] shouldCloseTo b.toStd.neg
  }

  property(s"${op("var1")} reverse node2") = forAll(genV1_RowEquivN2()) { case (a: Var1, b: N2) =>
    val g = op(a).reverse(b)
    g(a).get.asInstanceOf[N2].eval[T2] shouldCloseTo b.toStd.neg
  }

}


