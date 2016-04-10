package com.kogecoo.scalaad.test.node

import com.kogecoo.scalaad.graph.{N0, N1, N2}
import org.scalacheck.Prop.forAll
import org.scalacheck.{Gen, Properties}


trait UnaryOp0SpecBase extends NodeSpecBase { self: Properties =>

  import com.kogecoo.scalaad.impl.std.Implicits._
  import com.kogecoo.scalaad.test.helper.impl.std.Implicits._

  def op(a: N0): N0

  def opStr(argStr: String): String

  final def opWithN0Str: String = opStr("node0")

  final def opWithNV0Str: String = opStr("nonvar0")

  val genArgN0ForBase: Gen[N0] = genN0()

  val genArgNV0ForBase: Gen[N0] = genNV0()

  val genN0ForBase: Gen[N0] = genN0()

  val genN1ForBase: Gen[N1] = genN1()

  val genN2ForBase: Gen[N2] = genN2()

  val genNV0ForBase: Gen[N0] = genNV0()

  val genNV1ForBase: Gen[N1] = genNV1()

  val genNV2ForBase: Gen[N2] = genNV2()

  property(s"$opWithN0Str forward w.r.t node0") = forAll(genArgN0ForBase, genN0ForBase) { (a: N0, b: N0) =>
    op(a).forward[N0, N0](b).eval[T0] shouldCloseTo 0.0
  }

  property(s"$opWithN0Str forward w.r.t node1") = forAll(genArgN0ForBase, genN1ForBase) { (a: N0, b: N1) =>
    op(a).forward[N1, N1](b).eval[T1] shouldCloseTo zero1(b)
  }

  property(s"$opWithN0Str forward w.r.t node2") = forAll(genArgN0ForBase, genN2ForBase) { (a: N0, b: N2) =>
    op(a).forward[N2, N2](b).eval[T2] shouldCloseTo zero2(b)
  }

  property(s"$opWithNV0Str reverse node0") = forAll(genArgNV0ForBase, genN0ForBase) { (a: N0, b: N0) =>
    op(a).reverse(b).size == 0
  }

  property(s"$opWithNV0Str reverse node1") = forAll(genArgNV0ForBase, genN1ForBase) { (a: N0, b: N1) =>
    op(a).reverse(b).size == 0
  }

  property(s"$opWithNV0Str reverse node2") = forAll(genArgNV0ForBase, genN2ForBase) { (a: N0, b: N2) =>
    op(a).reverse(b).size == 0
  }

}


trait UnaryOp1SpecBase extends NodeSpecBase { self: Properties =>

  def op(a: N1): N1

  def opStr(a: String): String

  val opWithN1Str: String = opStr("node1")

  val opWithNV1Str: String = opStr("nonvar1")

  val genArgN1ForBase: Gen[N1] = genN1()

  val genArgNV1ForBase: Gen[N1] = genNV1()

  val genN0ForBase: Gen[N0] = genN0()

  val genN1ForBase: Gen[N1] = genN1()

  val genN2ForBase: Gen[N2] = genN2()

  val genNV0ForBase: Gen[N0] = genNV0()

  val genNV1ForBase: Gen[N1] = genNV1()

  val genNV2ForBase: Gen[N2] = genNV2()
/*
  property(s"$opWithN1Str forward w.r.t node0") = forAll(genArgN1ForBase, genN0()) { (a: N1, b: N0) =>
    op(a).forward[N0, N1](b).eval[T1] shouldCloseTo zero1(a)
  }

  property(s"$opWithN1Str forward w.r.t node1") = forAll(genArgN1ForBase, genN1()) { (a: N1, b: N1) =>
    op(a).forward[N1, N2](b).eval[T2] shouldCloseTo zero2(a, b)
  }

  property(s"$opWithNV1Str reverse node0") = forAll(genNV1ForFunc, genN0()) { (a: N1, b: N0) =>
    op(a).reverse(b).size == 0
  }

  property(s"$opWithNV1Str reverse node1") = forAll(genNV1ForFunc_N1) { case (a: N1, b: N1) =>
    op(a).reverse(b).size == 0
  }

  property(s"$opWithNV1Str reverse node2") = forAll(genNV1ForFunc_N2) { case (a: N1, b: N2) =>
    op(a).reverse(b).size == 0
  }
*/
}


