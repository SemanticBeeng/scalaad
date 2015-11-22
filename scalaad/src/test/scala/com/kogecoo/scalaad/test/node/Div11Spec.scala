package com.kogecoo.scalaad.test.node

import com.kogecoo.scalaad.graph._
import com.kogecoo.scalaad.impl.std.Implicits._
import com.kogecoo.scalaad.test.helper.impl.std.Implicits._
import com.kogecoo.scalaad.test.helper.impl.std.StdValueGen
import org.scalacheck.Prop.forAll
import org.scalacheck.{Gen, Properties}


object Div11Spec extends Properties("Div11") with NodeSpecBase {

  override val defaultMinValue = Some(-1e10)
  override val defaultMaxValue = Some( 1e10)

  val denomDomain = StdValueGen(
    defaultMinValue,
    defaultMaxValue,
    (x: Double) => x != -0.0 && x != 0.0
  )

  val N1_NonzeroN1: Gen[(N1, N1)] = for {
    first  <- genN1()
    second <- genNonzeroN1(first.shape, denomDomain)
  } yield (first, second)

  val N1_NonzeroN1_N1: Gen[(N1, N1, N1)] = for {
    first  <- genN1()
    second <- genNonzeroN1(first.shape, denomDomain)
    third  <- genN1(first.shape)
  } yield (first, second, third  )

  val V1_NonzeroN1: Gen[(N1, N1)] = for {
    first  <- genV1()
    second <- genNonzeroN1(first.shape, denomDomain)
  } yield (first, second)

  val N1_NonzeroV1: Gen[(N1, N1)] = for {
    first  <- genN1()
    second <- genV1(first.shape, denomDomain)
  } yield (first, second)

  val NonzeroV1_N1: Gen[(N1, N1)] = for {
    first  <- genV1(value = denomDomain)
    second <- genN1(first.shape)
  } yield (first, second)

  val NonzeroV1_N2: Gen[(N1, N2)] = for {
    first  <- genV1(value = denomDomain)
    second <- genN2(genS2(first.shape))
  } yield (first, second)

  val NV1_NonzeroNV1: Gen[(N1, N1)] = for {
    first  <- genNV1()
    second <- genNonzeroNV1(first.shape, denomDomain)
  } yield (first, second)

  val NV1_NonzeroNV1_N1: Gen[(N1, N1, N1)] = for {
    first  <- genNV1()
    second <- genNonzeroNV1(first.shape, denomDomain)
    third  <- genN1(first.shape)
  } yield (first, second, third)

  val NV1_NonzeroNV1_N2: Gen[(N1, N1, N2)] = for {
    first  <- genNV1()
    second <- genNonzeroNV1(first.shape, denomDomain)
    third  <- genN2(genS2(first.shape))
  } yield (first, second, third)

  val NV1_NonzeroV1: Gen[(N1, N1)] = for {
    first  <- genNV1()
    second <- genV1(first.shape, denomDomain)
  } yield (first, second)

  val NV1_NonzeroV1_N1: Gen[(N1, N1, N1)] = for {
    first   <- genNV1()
    second  <- genV1(first.shape, denomDomain)
    third   <- genN1(first.shape)
  } yield (first, second, third)

  val NV1_NonzeroV1_N2: Gen[(N1, N1, N2)] = for {
    first   <- genNV1()
    second  <- genV1(first.shape, denomDomain)
    third   <- genN2(genS2(first.shape))
  } yield (first, second, third)

  val V1_NonzeroNV1: Gen[(N1, N1)] = for {
    first  <- genV1()
    second <- genNonzeroNV1(first.shape, denomDomain)
  } yield (first, second)

  val V1_NonzeroNV1_N1: Gen[(N1, N1, N1)] = for {
    first   <- genV1()
    second  <- genNonzeroNV1(first.shape, denomDomain)
    third   <- genN1(first.shape)
  } yield (first, second, third)

  val V1_NonzeroNV1_N2: Gen[(N1, N1, N2)] = for {
    first   <- genV1()
    second  <- genNonzeroNV1(first.shape, denomDomain)
    third   <- genN2(genS2(first.shape))
  } yield (first, second, third)


  property("eval") = forAll(N1_NonzeroN1) { case (a: N1, b: N1) =>
    Div11(a, b).eval[T1] shouldCloseTo (a.toStd div b.toStd)
  }

  property("node1 / node1 forward w.r.t node0") = forAll(N1_NonzeroN1, genN0()) { case ((a: N1, b: N1), c: N0) =>
    Div11(a, b).forward[N0, N1](c).eval[T1] shouldCloseTo zero1(b)
  }

  property("node1 / node1 forward w.r.t node1") = forAll(N1_NonzeroN1_N1) { case (a: N1, b: N1, c: N1) =>
    Div11(a, b).forward[N1, N2](c).eval[T2] shouldCloseTo zero2(a, b)
  }

  property("var1 / node1 forward w.r.t left") = forAll(V1_NonzeroN1) { case (a: Var1, b: N1) =>
    Div11(a, b).forward[N1, N2](a).eval[T2] shouldCloseTo diag(one1(b) div b.toStd)
  }

  property("node1 / var1 forward w.r.t right") = forAll(N1_NonzeroV1) { case (a: N1, b: Var1) =>
    Div11(a, b).forward[N1, N2](b).eval[T2] shouldCloseTo diag(a.toStd.neg div (b.toStd mul b.toStd))
  }

  //  FIXME: the differentiation result of (var / var) should be zero
  // but following code doesn't produce it because of a floating-point precision issue.
  property("var1 / var1 forward w.r.t self") = forAll(genV1(value = denomDomain)) { case a: Var1 =>
    val x = a.toStd

    val l = one1(a) div x
    val r = x.neg div (x mul x)

    Div11(a, a).forward[N1, N2](a).eval[T2] shouldCloseTo diag(l add r) // zero2(a, a)
  }

  property("nonvar1 / nonvar1 reverse node0") = forAll(NV1_NonzeroNV1, genN0()) { case ((a: N1, b: N1), c: N0) =>
    Div11(a, b).reverse(c).size == 0
  }

  property("nonvar1 / nonvar1 reverse node1") = forAll(NV1_NonzeroNV1_N1) { case (a: N1, b: N1, c: N1) =>
    Div11(a, b).reverse(c).size == 0
  }

  property("nonvar1 / nonvar1 reverse node2") = forAll(NV1_NonzeroNV1_N2) { case (a: N1, b: N1, c: N2) =>
    Div11(a, b).reverse(c).size == 0
  }

  property("var1 / nonvar1 reverse node0") = forAll(V1_NonzeroNV1, genN0()) { case ((a: Var1, b: N1), c: N0) =>
    val g = Div11(a, b).reverse(c)
    g(a).get.asInstanceOf[N1].eval[T1] shouldCloseTo b.toStd.map(c.toStd / _)
  }

  property("var1 / nonvar1 reverse node1") = forAll(V1_NonzeroNV1_N1) { case (a: Var1, b: N1, c: N1) =>
    val g = Div11(a, b).reverse(c)
    g(a).get.asInstanceOf[N1].eval[T1] shouldCloseTo (c.toStd div b.toStd)
  }

  property("var1 / nonvar1 reverse node2") = forAll(V1_NonzeroNV1_N2) { case (a: Var1, b: N1, c: N2) =>
    val g = Div11(a, b).reverse(c)
    g(a).get.asInstanceOf[N2].eval[T2] shouldCloseTo (c.toStd colDiv b.toStd)
  }

  property("nonvar1 / var1 reverse node0") = forAll(NV1_NonzeroV1, genN0()) { case ((a: N1, b: Var1), c: N0) =>
    val g = Div11(a, b).reverse(c)
    val beval = b.toStd
    g(b).get.asInstanceOf[N1].eval[T1] shouldCloseTo ((a.toStd.neg mul c.toStd) div (beval mul beval))
  }

  property("nonvar1 / var1 reverse node1") = forAll(NV1_NonzeroV1_N1) { case (a: N1, b: Var1, c: N1) =>
    val g = Div11(a, b).reverse(c)
    val beval = b.toStd
    g(b).get.asInstanceOf[N1].eval[T1] shouldCloseTo ((a.toStd.neg mul c.toStd) div (beval mul beval))
  }

  property("nonvar1 / var1 reverse node2") = forAll(NV1_NonzeroV1_N2) { case (a: N1, b: Var1, c: N2) =>
    val g = Div11(a, b).reverse(c)
    val beval = b.toStd
    g(b).get.asInstanceOf[N2].eval[T2] shouldCloseTo ((c.toStd.neg colMul a.toStd) colDiv (beval mul beval))
  }

  //  FIXME: the differentiation result of (var / var) should be zero
  // but following code doesn't produce it because of a floating-point precision issue.
  property("var1 / var1 reverse node0") = forAll(genV1(value = denomDomain), genN0()) { case (a: Var1, b: N0) =>
    val g = Div11(a, a).reverse(b)

    val x = a.toStd
    val y = b.toStd

    val l = x.elementwise(y / _)
    val r = x.elementwise(-y * _) div (x mul x)

    g(a).get.asInstanceOf[N1].eval[T1] shouldCloseTo (l add r) //zero1(a)
  }

  //  FIXME: the differentiation result of (var / var) should be zero
  // but following code doesn't produce it because of a floating-point precision issue.
  property("var1 / var1 reverse node1") = forAll(NonzeroV1_N1) { case (a: Var1, b: N1) =>
    val g = Div11(a, a).reverse(b)

    val x = a.toStd
    val y = b.toStd

    val l = y div x
    val r = (y.neg mul x) div (x mul x)

    g(a).get.asInstanceOf[N1].eval[T1] shouldCloseTo (l add r) //zero1(a)
  }

  //  FIXME: the differentiation result of (var / var) should be zero
  // but following code doesn't produce it because of a floating-point precision issue.
  property("var1 / var1 reverse node2") = forAll(NonzeroV1_N2) { case (a: Var1, b: N2) =>
    val g = Div11(a, a).reverse(b)

    val x = a.toStd
    val y = b.toStd

    val l = y colDiv x
    val r = (y.neg colMul x) colDiv (x mul x)

    g(a).get.asInstanceOf[N2].eval[T2] shouldCloseTo (l add r)//zero2(b)
  }

}
