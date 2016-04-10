package com.kogecoo.scalaad.test.node

import com.kogecoo.scalaad.graph.{N0, N1}
import org.scalacheck.Properties


trait BinaryOp00SpecBase extends NodeSpecBase { self: Properties =>

  def op(a: N0, b: N0): N0

  def opStr(a: String, b: String): String

  val opWithN0_N0Str: String = opStr("node0", "node0")

  val opWithNV0_NV0Str: String = opStr("nonvar0", "nonvar0")
}


trait BinaryOp11SpecBase extends NodeSpecBase { self: Properties =>

  def op(a: N1, b: N1): N1

  def opStr(a: String, b: String): String

  val opWithN1_N1Str: String = opStr("node1", "node1")

  val opWithNV1_NV1Str: String = opStr("nonvar1", "nonvar1")
}

