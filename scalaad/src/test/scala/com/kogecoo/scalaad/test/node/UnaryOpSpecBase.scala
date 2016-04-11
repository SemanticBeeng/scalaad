package com.kogecoo.scalaad.test.node

import com.kogecoo.scalaad.graph.{N0, N1, N2}
import org.scalacheck.Prop.forAll
import org.scalacheck.{Gen, Properties}


trait UnaryOp0SpecBase extends NodeSpecBase { self: Properties =>

  import com.kogecoo.scalaad.impl.std.Implicits._
  import com.kogecoo.scalaad.test.helper.impl.std.Implicits._

  def op(a: N0): N0

  def op(argStr: String): String

  def genArgN0ForSpecBase: Gen[N0] = genN0()

  def genArgNV0ForSpecBase: Gen[N0] = genNV0()

  def genN0ForSpecBase: Gen[N0] = genN0()

  def genN1ForSpecBase: Gen[N1] = genN1()

  def genN2ForSpecBase: Gen[N2] = genN2()


  property(s"${op("node0")} forward w.r.t node0") = forAll(genArgN0ForSpecBase, genN0ForSpecBase) { (a: N0, b: N0) =>
    op(a).forward[N0, N0](b).eval[T0] shouldCloseTo 0.0
  }

  property(s"${op("node0")} forward w.r.t node1") = forAll(genArgN0ForSpecBase, genN1ForSpecBase) { (a: N0, b: N1) =>
    op(a).forward[N1, N1](b).eval[T1] shouldCloseTo zero1(b)
  }

  property(s"${op("node0")} forward w.r.t node2") = forAll(genArgN0ForSpecBase, genN2ForSpecBase) { (a: N0, b: N2) =>
    op(a).forward[N2, N2](b).eval[T2] shouldCloseTo zero2(b)
  }

  property(s"${op("nonvar0")} reverse node0") = forAll(genArgNV0ForSpecBase, genN0ForSpecBase) { (a: N0, b: N0) =>
    op(a).reverse(b).size == 0
  }

  property(s"${op("nonvar0")} reverse node1") = forAll(genArgNV0ForSpecBase, genN1ForSpecBase) { (a: N0, b: N1) =>
    op(a).reverse(b).size == 0
  }

  property(s"${op("nonvar0")} reverse node2") = forAll(genArgNV0ForSpecBase, genN2ForSpecBase) { (a: N0, b: N2) =>
    op(a).reverse(b).size == 0
  }

}


trait UnaryOp1SpecBase extends NodeSpecBase { self: Properties =>

  import com.kogecoo.scalaad.impl.std.Implicits._
  import com.kogecoo.scalaad.test.helper.impl.std.Implicits._


  def op(a: String): String

  def op(a: N1): N1

  def genArgN1ForSpecBase: Gen[N1] = genN1()

  def genArgNV1ForSpecBase: Gen[N1] = genNV1()

  def genN0ForSpecBase: Gen[N0] = genN0()

  def genN1ForSpecBase: Gen[N1] = genN1()

  def genArgNV1_N1_ForSpecBase: Gen[(N1, N1)] = genNV1_N1()

  def genArgNV1_N2_ForSpecBase: Gen[(N1, N2)] = genNV1_RowEquivN2()


  property(s"${op("node0")} forward w.r.t node0") = forAll(genArgN1ForSpecBase, genN0ForSpecBase) { (a: N1, b: N0) =>
    op(a).forward[N0, N1](b).eval[T1] shouldCloseTo zero1(a)
  }

  property(s"${op("node0")} forward w.r.t node1") = forAll(genArgN1ForSpecBase, genN1ForSpecBase) { (a: N1, b: N1) =>
    op(a).forward[N1, N2](b).eval[T2] shouldCloseTo zero2(a, b)
  }

  property(s"${op("nonvar0")} reverse node0") = forAll(genArgNV1ForSpecBase, genN0ForSpecBase) { (a: N1, b: N0) =>
    op(a).reverse(b).size == 0
  }

  property(s"${op("nonvar0")} reverse node1") = forAll(genArgNV1_N1_ForSpecBase) { case (a: N1, b: N1) =>
    op(a).reverse(b).size == 0
  }

  property(s"${op("nonvar0")} reverse node2") = forAll(genArgNV1_N2_ForSpecBase) { case (a: N1, b: N2) =>
    op(a).reverse(b).size == 0
  }

}

