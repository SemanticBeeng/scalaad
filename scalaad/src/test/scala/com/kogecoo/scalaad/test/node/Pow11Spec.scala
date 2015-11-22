package com.kogecoo.scalaad.test.node

import com.kogecoo.scalaad.graph._
import com.kogecoo.scalaad.impl.std.Implicits._
import com.kogecoo.scalaad.test.helper.impl.std.Implicits._
import com.kogecoo.scalaad.test.helper.impl.std.StdValueGen
import org.scalacheck.Prop.forAll
import org.scalacheck.{Gen, Properties}


object Pow11Spec extends Properties("Pow11") with NodeSpecBase {

  override val defaultMinValue = Some(-100.0)
  override val defaultMaxValue = Some( 100.0)

  val expDomain = StdValueGen(
    defaultMinValue,
    defaultMaxValue,
    (x: Double) => x != -0.0 && x != 0.0
  )

  val positiveBaseDomain = StdValueGen(
    Some(0.0),
    defaultMaxValue,
    (x: Double) => x != -0.0 && x != 0.0
  )

  val NonzeroN1_N1: Gen[(N1, N1)] = for {
    first  <- genNonzeroN1(value = positiveBaseDomain)
    second <- genN1(first.shape, expDomain)
  } yield (first, second)

  val NonzeroN1_N1_N1: Gen[(N1, N1, N1)] = for {
    first  <- genNonzeroN1(value = positiveBaseDomain)
    second <- genNonzeroN1(first.shape, expDomain)
    third  <- genN1(first.shape)
  } yield (first, second, third)

  val NonzeroN1_V1: Gen[(N1, N1)] = for {
    first  <- genNonzeroN1(value = positiveBaseDomain)
    second <- genV1(first.shape, expDomain)
  } yield (first, second)

  val NonzeroNV1_NV1: Gen[(N1, N1)] = for {
    first  <- genNonzeroNV1(value = positiveBaseDomain)
    second <- genNV1(first.shape, expDomain)
  } yield (first, second)

  val NonzeroNV1_NV1_N1: Gen[(N1, N1, N1)] = for {
    first  <- genNonzeroNV1(value = positiveBaseDomain)
    second <- genNV1(first.shape, expDomain)
    third  <- genN1(first.shape)
  } yield (first, second, third)

  val NonzeroNV1_NV1_N2: Gen[(N1, N1, N2)] = for {
    first  <- genNonzeroNV1(value = positiveBaseDomain)
    second <- genNV1(first.shape, expDomain)
    third  <- genN2(genS2(first.shape))
  } yield (first, second, third)

  val NonzeroNV1_V1: Gen[(N1, N1)] = for {
    first  <- genNonzeroNV1(value = positiveBaseDomain)
    second <- genV1(first.shape, expDomain)
  } yield (first, second)

  val NonzeroNV1_V1_N1: Gen[(N1, N1, N1)] = for {
    first   <- genNonzeroNV1(value = positiveBaseDomain)
    second  <- genV1(first.shape, expDomain)
    third   <- genN1(first.shape)
  } yield (first, second, third)

  val NonzeroNV1_V1_N2: Gen[(N1, N1, N2)] = for {
    first   <- genNonzeroNV1(value = positiveBaseDomain)
    second  <- genV1(first.shape, expDomain)
    third   <- genN2(genS2(first.shape))
  } yield (first, second, third)

  val NonzeroV1_NV1_N2: Gen[(N1, N1, N2)] = for {
    first   <- genV1(value = positiveBaseDomain)
    second  <- genNonzeroNV1(first.shape, expDomain)
    third   <- genN2(genS2(first.shape))
  } yield (first, second, third)

  def op(a: N1, b: N1): N1 = Pow11(a, b)

  def pow(a: T1, b: T1): T1 = a.elementwise(b, math.pow)

  def log(a: T1): T1 = a.elementwise(math.log)

  property("eval") = forAll(NonzeroN1_N1) { case (a: N1, b: N1) =>
    op(a, b).eval[T1] shouldCloseTo pow(a.toStd, b.toStd)
  }

  property("pow(node1, node1) forward w.r.t node0") = forAll(NonzeroN1_N1, genN0()) { case ((a: N1, b: N1), c: N0) =>
    op(a, b).forward[N0, N1](c).eval[T1] shouldCloseTo zero1(b)
  }

  property("pow(node1, node1) forward w.r.t node1") = forAll(NonzeroN1_N1_N1) { case (a: N1, b: N1, c: N1) =>
    op(a, b).forward[N1, N2](c).eval[T2] shouldCloseTo zero2(a, b)
  }

  property("pow(var1, node1) forward w.r.t left") = forAll(genV1_N1(positiveBaseDomain, genDefaultDomain)) { case (a: Var1, b: N1) =>
    val x = a.toStd
    val y = b.toStd
    op(a, b).forward[N1, N2](a).eval[T2] shouldCloseTo diag(y mul pow(x, y sub 1.0))
  }

  property("pow(node1, var1) forward w.r.t right") = forAll(NonzeroN1_V1) { case (a: N1, b: Var1) =>
    val x = a.toStd
    val y = b.toStd
    op(a, b).forward[N1, N2](b).eval[T2] shouldCloseTo diag(log(x) mul pow(x, y))
  }

  property("pow(var1, var1) forward w.r.t self") = forAll(genV1(value = positiveBaseDomain)) { case a: Var1 =>
    val x = a.toStd
    op(a, a).forward[N1, N2](a).eval[T2] shouldCloseTo (diag(x mul pow(x, x sub 1.0)) add diag(log(x) mul pow(x, x)))
  }

  property("pow(nonvar1, nonvar1) reverse node0") = forAll(NonzeroNV1_NV1, genN0()) { case ((a: N1, b: N1), c: N0) =>
    op(a, b).reverse(c).size == 0
  }

  property("pow(nonvar1, nonvar1) reverse node1") = forAll(NonzeroNV1_NV1_N1) { case (a: N1, b: N1, c: N1) =>
    op(a, b).reverse(c).size == 0
  }

  property("pow(nonvar1, nonvar1) reverse node2") = forAll(NonzeroNV1_NV1_N2) { case (a: N1, b: N1, c: N2) =>
    op(a, b).reverse(c).size == 0
  }

  property("pow(nonvar1, nonvar1) reverse node0") = forAll(NonzeroNV1_NV1, genN0()) { case ((a: N1, b: N1), c: N0) =>
    op(a, b).reverse(c).size == 0
  }

  property("pow(nonvar1, nonvar1) reverse node1") = forAll(NonzeroNV1_NV1_N1) { case (a: N1, b: N1, c: N1) =>
    op(a, b).reverse(c).size == 0
  }

  property("pow(nonvar1, nonvar1) reverse node2") = forAll(NonzeroNV1_NV1_N2) { case (a: N1, b: N1, c: N2) =>
    op(a, b).reverse(c).size == 0
  }

  property("pow(var1, nonvar1) reverse node0") = forAll(genV1_NV1(positiveBaseDomain, expDomain), genN0()) { case ((a: Var1, b: N1), c: N0) =>
    val x = a.toStd
    val y = b.toStd
    val z = c.toStd
    val g = op(a, b).reverse(c)
    g(a).get.asInstanceOf[N1].eval[T1] shouldCloseTo ((y mul pow(x, y sub 1.0)) mul z)
  }

  property("pow(var1, nonvar1) reverse node1") = forAll(genV1_NV1_N1(positiveBaseDomain, expDomain, genDefaultDomain)) { case (a: Var1, b: N1, c: N1) =>
    val x = a.toStd
    val y = b.toStd
    val z = c.toStd
    val g = op(a, b).reverse(c)
    g(a).get.asInstanceOf[N1].eval[T1] shouldCloseTo ((y mul pow(x, y sub 1.0)) mul z)
  }

  property("pow(var1, nonvar1) reverse node2") = forAll(NonzeroV1_NV1_N2) { case (a: Var1, b: N1, c: N2) =>
    val x = a.toStd
    val y = b.toStd
    val z = c.toStd
    val g = op(a, b).reverse(c)
    g(a).get.asInstanceOf[N2].eval[T2] shouldCloseTo (z colMul (y mul pow(x, y sub 1.0)))
  }

  property("pow(nonvar1, var1) reverse node0") = forAll(NonzeroNV1_V1, genN0()) { case ((a: N1, b: Var1), c: N0) =>
    val x = a.toStd
    val y = b.toStd
    val z = c.toStd
    val g = op(a, b).reverse(c)
    g(b).get.asInstanceOf[N1].eval[T1] shouldCloseTo ((log(x) mul pow(x, y)) mul z)
  }

  property("pow(nonvar1, var1) reverse node1") = forAll(NonzeroNV1_V1_N1) { case (a: N1, b: Var1, c: N1) =>
    val x = a.toStd
    val y = b.toStd
    val z = c.toStd
    val g = op(a, b).reverse(c)
    g(b).get.asInstanceOf[N1].eval[T1] shouldCloseTo ((log(x) mul pow(x, y)) mul z)
  }

  property("pow(nonvar1, var1) reverse node2") = forAll(NonzeroNV1_V1_N2) { case (a: N1, b: Var1, c: N2) =>
    val x = a.toStd
    val y = b.toStd
    val z = c.toStd
    val g = op(a, b).reverse(c)
    g(b).get.asInstanceOf[N2].eval[T2] shouldCloseTo (z colMul (log(x) mul pow(x, y)))
  }

  property("pow(var1, var1) reverse node0") = forAll(genV1(value = positiveBaseDomain), genN0()) { case (a: Var1, b: N0) =>
    val x = a.toStd
    val y = b.toStd

    val l = x mul pow(x, x sub 1.0)
    val r = log(x) mul pow(x, x)

    val g = op(a, a).reverse(b)
    g(a).get.asInstanceOf[N1].eval[T1] shouldCloseTo ((l add r) mul y)
  }

  property("pow(var1, var1) reverse node1") = forAll(genV1_N1(positiveBaseDomain, genDefaultDomain)) { case (a: Var1, b: N1) =>
    val x = a.toStd
    val y = b.toStd

    val l = x mul pow(x, x sub 1.0)
    val r = log(x) mul pow(x, x)

    val g = op(a, a).reverse(b)
    g(a).get.asInstanceOf[N1].eval[T1] shouldCloseTo ((l add r) mul y)
  }

  property("pow(var1, var1) reverse node2") = forAll(genV1_RowEquivN2(positiveBaseDomain, genDefaultDomain)) { case (a: Var1, b: N2) =>
    val x = a.toStd
    val y = b.toStd

    val l = x mul pow(x, x sub 1.0)
    val r = log(x) mul pow(x, x)

    val g = op(a, a).reverse(b)
    g(a).get.asInstanceOf[N2].eval[T2] shouldCloseTo (y colMul (l add r))
  }

}
