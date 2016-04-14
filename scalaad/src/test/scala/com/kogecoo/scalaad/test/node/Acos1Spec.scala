package com.kogecoo.scalaad.test.node

import com.kogecoo.scalaad.graph._
import com.kogecoo.scalaad.impl.std.Implicits._
import com.kogecoo.scalaad.test.helper.impl.std.Implicits._
import com.kogecoo.scalaad.test.helper.impl.std.StdValueGen
import org.scalacheck.Prop.forAll
import org.scalacheck.{Gen, Properties}


object Acos1Spec extends Properties("Acos1") with UnaryOp1SpecBase {

  private[this] def deriv(x: Double): Double = -1.0 / math.sqrt(1.0 - x * x)

  override def defaultMinValue = Some(-1e10)
  override def defaultMaxValue = Some( 1e10)

  def domain = StdValueGen(
    Some(-1.0),
    Some(1.0),
    (x: Double) => x != -1.0 && x != 1.0
  )

  // exclude One1 node
  def genArgV1: Gen[Var1] = genV1(value = domain)

  def genArgN1: Gen[N1] = Gen.oneOf(
    genV1(value = domain),
    genConst1(value = domain),
    genHalf1(),
    genZero1()
  )

  def genArgNV1: Gen[N1] = Gen.oneOf(
    genConst1(value = domain),
    genHalf1(),
    genZero1()
  )

  def genArgNV1_N1: Gen[(N1, N1)] = for {
    first  <- genArgNV1
    second <- n1gen.genNode1(first.shape, genDefaultDomain)
  } yield (first, second)

  def genArgNV1_N2: Gen[(N1, N2)] = for {
    first  <- genArgNV1
    s2     =  genS2(first.shape._1)
    second <- n2gen.genNode2(s2, genDefaultDomain)
  } yield (first, second)

  def genArgV1_N1: Gen[(Var1, N1)] = for {
    first  <- genArgV1
    second <- n1gen.genNode1(first.shape, genDefaultDomain)
  } yield (first, second)

  def genArgV1_N2: Gen[(Var1, N2)] = for {
    first  <- genArgV1
    s2     =  genS2(first.shape._1)
    second <- n2gen.genNode2(s2, genDefaultDomain)
  } yield (first, second)


  override def op(a: N1): N1 = Acos1(a)

  override def op(argStr: String): String = s"acos($argStr)"

  override def genArgN1ForSpecBase: Gen[N1] = genArgN1

  override def genArgNV1ForSpecBase: Gen[N1] = genArgNV1

  override def genArgNV1_N1_ForSpecBase: Gen[(N1, N1)] = genArgNV1_N1

  override def genArgNV1_N2_ForSpecBase: Gen[(N1, N2)] = genArgNV1_N2

  property("eval") = forAll(genArgN1) { (a: N1) =>
    op(a).eval[T1] shouldCloseTo a.toStd.map(math.acos)
  }

  property(s"${op("var1")} forward w.r.t self") = forAll(genArgV1) { (a: Var1) =>
    op(a).forward[N1, N2](a).eval[T2] shouldCloseTo diag(a.toStd.map(deriv))
  }

  property(s"${op("var1")} reverse node0") = forAll(genArgV1, genN0()) { (a: Var1, b: N0) =>
    val g = op(a).reverse(b)
    g(a).get.asInstanceOf[N1].eval[T1] shouldCloseTo (a.toStd.map(deriv) mul b.toStd)
  }

  property(s"${op("var1")} reverse node1") = forAll(genArgV1_N1) { case (a: Var1, b: N1) =>
    val g = op(a).reverse(b)
    g(a).get.asInstanceOf[N1].eval[T1] shouldCloseTo (a.toStd.map(deriv) mul b.toStd)
  }

  property(s"${op("var1")} reverse node2") = forAll(genArgV1_N2) { case (a: Var1, b: N2) =>
    val g = op(a).reverse(b)
    g(a).get.asInstanceOf[N2].eval[T2] shouldCloseTo (b.toStd colMul a.toStd.map(deriv))
  }

}


