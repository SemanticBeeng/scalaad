package com.kogecoo.scalaad.test.node

import com.kogecoo.scalaad.graph._
import com.kogecoo.scalaad.impl.std.Implicits._
import com.kogecoo.scalaad.test.helper.impl.std.Implicits._
import org.scalacheck.Prop.forAll
import org.scalacheck.Properties


object Ln1Spec extends Properties("Ln1") with UnaryOp1SpecBase {

  override def defaultMinValue = Some(0.0)
  override def defaultMaxValue = Some(100.0)
  override def defaultValueConstraint = (x: Double) => x != -0.0 && x != 0.0

  override def op(a: N1): N1 = Ln1(a)

  override def op(argStr: String): String = s"ln($argStr)"

  property("eval") = forAll(genNonzeroN1()) { (a: N1) =>
    op(a).eval[T1] shouldCloseTo a.toStd.map(math.log)
  }

  property(s"${op("var1")} forward w.r.t self") = forAll(genV1()) { (a: Var1) =>
    op(a).forward[N1, N2](a).eval[T2] shouldCloseTo diag(a.toStd.map(1 / _))
  }

  property(s"${op("var1")} reverse node0") = forAll(genV1(), genN0()) { (a: Var1, b: N0) =>
    val g = op(a).reverse(b)
    g(a).get.asInstanceOf[N1].eval[T1] shouldCloseTo a.toStd.map(b.toStd / _)
  }

  property(s"${op("var1")} reverse node1") = forAll(genV1_N1()) { case (a: Var1, b: N1) =>
    val g = op(a).reverse(b)
    g(a).get.asInstanceOf[N1].eval[T1] shouldCloseTo (a.toStd.map(1 / _) mul b.toStd)
  }

  property(s"${op("var1")} reverse node2") = forAll(genV1_RowEquivN2()) { case (a: Var1, b: N2) =>
    val g = op(a).reverse(b)
    g(a).get.asInstanceOf[N2].eval[T2] shouldCloseTo (b.toStd colMul a.toStd.map(1 / _))
  }

}


