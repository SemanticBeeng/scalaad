package com.kogecoo.scalaad.test.node

import com.kogecoo.scalaad.graph._
import com.kogecoo.scalaad.impl.std.Implicits._
import com.kogecoo.scalaad.test.helper.impl.std.Implicits._
import com.kogecoo.scalaad.test.helper.impl.std.StdValueGen
import org.scalacheck.Prop.forAll
import org.scalacheck.{Gen, Properties}


object Acos2Spec extends Properties("Acos1") with UnaryOp2SpecBase {

  private[this] def deriv(x: Double): Double = -1.0 / math.sqrt(1.0 - x * x)

  override def defaultMinValue = Some(-1e10)
  override def defaultMaxValue = Some( 1e10)

  def domain = StdValueGen(
    Some(-1.0),
    Some(1.0),
    (x: Double) => x != -1.0 && x != 1.0
  )

  // exclude One1 node
  def genArgV2: Gen[Var2] = genV2(value = domain)

  def genArgN2: Gen[N2] = Gen.oneOf(
    genV2(value = domain),
    genConst2(value = domain),
    genHalf2(),
    genZero2()
  )

  def genArgNV2: Gen[N2] = Gen.oneOf(
    genConst2(value = domain),
    genHalf2(),
    genZero2()
  )

  def genArgNV2_RowEquivN1: Gen[(N2, N1)] = {
    for {
      first  <- genArgNV2
      s1     =  genS1(first.shape._1)
      second <- n1gen.genNode1(s1, genDefaultDomain)
    } yield (first, second)
  }

  def genArgNV2_N2: Gen[(N2, N2)] = {
    for {
      first  <- genArgNV2
      second <- n2gen.genNode2(first.shape, genDefaultDomain)
    } yield (first, second)
  }

  def genArgV2_RowEquivN1: Gen[(Var2, N1)] = {
    for {
      first  <- genArgV2
      s1     =  genS1(first.shape._1)
      second <- n1gen.genNode1(s1, genDefaultDomain)
    } yield (first, second)
  }

  def genArgV2_N2: Gen[(Var2, N2)] = {
    for {
      first  <- genArgV2
      second <- n2gen.genNode2(first.shape, genDefaultDomain)
    } yield (first, second)
  }

  override def op(a: String): String = s"acos($a)"

  override def op(a: N2): N2 = Acos2(a)

  override def genArgN2ForSpecBase: Gen[N2] = genArgN2

  override def genArgNV2ForSpecBase: Gen[N2] = genArgNV2

  override def genArgNV2_RowEquivN1_ForSpecBase: Gen[(N2, N1)] = genArgNV2_RowEquivN1

  property("eval") = forAll(genArgN2) { (a: N2) =>
    op(a).eval[T2] shouldCloseTo a.toStd.map(_.map(math.acos))
  }

  property(s"${op("var2")} reverse node0") = forAll(genArgV2, genN0()) { (a: Var2, b: N0) =>
    val g = op(a).reverse(b)
    g(a).get.asInstanceOf[N2].eval[T2] shouldCloseTo (a.toStd.map(_.map(deriv)) mul b.toStd)
  }

  property(s"${op("var2")} reverse node1") = forAll(genArgV2_RowEquivN1) { case (a: Var2, b: N1) =>
    val g = op(a).reverse(b)
    g(a).get.asInstanceOf[N2].eval[T2] shouldCloseTo (a.toStd.map(_.map(deriv)) rowMul b.toStd)
  }

  property(s"${op("var2")} reverse node2") = forAll(genArgV2_N2) { case (a: Var2, b: N2) =>
    val g = op(a).reverse(b)
    g(a).get.asInstanceOf[N2].eval[T2] shouldCloseTo (b.toStd mul a.toStd.map(_.map(deriv)))
  }

}
