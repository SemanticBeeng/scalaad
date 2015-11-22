package com.kogecoo.scalaad.test.node

import com.kogecoo.scalaad.graph._
import com.kogecoo.scalaad.impl.std.Implicits._
import com.kogecoo.scalaad.test.helper.impl.std.Implicits._
import org.scalacheck.Prop.forAll
import org.scalacheck.Properties


object Add11Spec extends Properties("Add11") with NodeSpecBase {

  override val defaultMinValue = Some(-1e100)
  override val defaultMaxValue = Some( 1e100)

  property("eval") = forAll(genN1_N1()) { case (a: N1, b: N1) =>
    Add11(a, b).eval[T1] shouldCloseTo a.toStd.zip(b.toStd).map { case (x, y) => x + y }
  }

  property("node1 + node1 forward w.r.t node0") = forAll(genN1_N1(), genN0()) { case ((a: N1, b: N1), c: N0) =>
    Add11(a, b).forward[N0, N1](c).eval[T1] shouldCloseTo zero1(b)
  }

  property("node1 + node1 forward w.r.t node1") = forAll(genN1_N1_N1()) { case (a: N1, b: N1, c: N1) =>
    Add11(a, b).forward[N1, N2](c).eval[T2] shouldCloseTo zero2(a, b)
  }

  property("var1 + node1 forward w.r.t left") = forAll(genV1_N1()) { case (a: Var1, b: N1) =>
    Add11(a, b).forward[N1, N2](a).eval[T2] shouldCloseTo eye(a, b)
  }

  property("node1 + var1 forward w.r.t right") = forAll(genN1_V1()) { case (a: N1, b: Var1) =>
    Add11(a, b).forward[N1, N2](b).eval[T2] shouldCloseTo eye(a, b)
  }

  property("var1 + var1 forward w.r.t self") = forAll(genV1()) { (a: Var1) =>
    Add11(a, a).forward[N1, N2](a).eval[T2] shouldCloseTo (eye(a, a) mul 2.0)
  }


  property("nonvar1 + nonvar1 reverse node0") = forAll(genNV1_NV1(), genN0()) { case ((a: N1, b: N1), c: N0) =>
    Add11(a, b).reverse(c).size == 0
  }

  property("nonvar1 + nonvar1 reverse node1") = forAll(genNV1_NV1_N1()) { case (a: N1, b: N1, c: N1) =>
    Add11(a, b).reverse(c).size == 0
  }

  property("nonvar1 + nonvar1 reverse node2") = forAll(genNV1_NV1_RowEquivN2()) { case (a: N1, b: N1, c: N2) =>
    Add11(a, b).reverse(c).size == 0
  }

  property("var1 + nonvar1 reverse node0") = forAll(genV1_NV1(), genN0()) { case ((a: Var1, b: N1), c: N0) =>
    val g = Add11(a, b).reverse(c)
    g(a).get.asInstanceOf[N1].eval[T1] shouldCloseTo (one1(a) mul c.toStd)
  }

  property("var1 + nonvar1 reverse node1") = forAll(genV1_NV1_N1()) { case(a: Var1, b: N1, c: N1) =>
    val g = Add11(a, b).reverse(c)
    g(a).get.asInstanceOf[N1].eval[T1] shouldCloseTo c.toStd
  }

  property("var1 + nonvar1 reverse node2") = forAll(genV1_NV1_RowEquivN2()) { case (a: Var1, b: N1, c: N2) =>
    val g = Add11(a, b).reverse(c)
    g(a).get.asInstanceOf[N2].eval[T2] shouldCloseTo c.toStd
  }

  property("nonvar1 + var1 reverse node0") = forAll(genNV1_V1(), genN0()) { case ((a: N1, b: Var1), c: N0) =>
    val g = Add11(a, b).reverse(c)
    g(b).get.asInstanceOf[N1].eval[T1] shouldCloseTo (one1(b) mul c.toStd)
  }

  property("nonvar1 + var1 reverse node1") = forAll(genNV1_V1_N1()) { case (a: N1, b: Var1, c: N1) =>
    val g = Add11(a, b).reverse(c)
    g(b).get.asInstanceOf[N1].eval[T1] shouldCloseTo c.toStd
  }

  property("nonvar1 + var1 reverse node2") = forAll(genNV1_V1_RowEquivN2()) { case (a: N1, b: Var1, c: N2) =>
    val g = Add11(a, b).reverse(c)
    g(b).get.asInstanceOf[N2].eval[T2] shouldCloseTo c.toStd
  }

  property("var1 + var1 reverse node0") = forAll(genV1(), genN0()) { (a: Var1, b: N0) =>
    val g = Add11(a, a).reverse(b)
    g(a).get.asInstanceOf[N1].eval[T1] shouldCloseTo (one1(a) mul (b.toStd * 2.0))
  }

  property("var1 + var1 reverse node1") = forAll(genV1_N1()) { case (a: Var1, b: N1) =>
    val g = Add11(a, a).reverse(b)
    g(a).get.asInstanceOf[N1].eval[T1] shouldCloseTo (b.toStd mul 2.0)
  }

  property("var1 + var1 reverse node2") = forAll(genV1_RowEquivN2()) { case (a: Var1, b: N2) =>
    val g = Add11(a, a).reverse(b)
    g(a).get.asInstanceOf[N2].eval[T2] shouldCloseTo (b.toStd mul 2.0)
  }

}


