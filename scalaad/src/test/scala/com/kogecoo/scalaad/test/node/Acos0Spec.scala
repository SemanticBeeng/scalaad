package com.kogecoo.scalaad.test.node

import com.kogecoo.scalaad.graph._
import com.kogecoo.scalaad.impl.std.Implicits._
import com.kogecoo.scalaad.test.helper.impl.std.Implicits._
import com.kogecoo.scalaad.test.helper.impl.std.StdValueGen
import org.scalacheck.Prop.forAll
import org.scalacheck.{Gen, Properties}


object Acos0Spec extends Properties("Acos0") with UnaryOp0SpecBase {

  override def defaultMinValue = Some(-1e10)
  override def defaultMaxValue = Some( 1e10)

  def domain = StdValueGen(
    Some(-1.0),
    Some(1.0),
    (x: Double) => x != -1.0 && x != 1.0
  )

  // exclude One0 node
  def genArgN0: Gen[N0] = Gen.oneOf(
    genV0(domain),
    genConst0(domain),
    genHalf0(),
    genZero0()
  )

  def genArgNV0: Gen[N0]  = Gen.oneOf(
    genConst0(domain),
    genHalf0(),
    genZero0()
  )

  override def op(a: N0): N0 = Acos0(a)

  override def op(argStr: String): String = s"acos($argStr)"

  override def genArgN0ForSpecBase: Gen[N0] = genArgN0

  override def genArgNV0ForSpecBase: Gen[N0] = genArgNV0


  private[this] def deriv(x: Double): Double = -1.0 / math.sqrt(1.0 - x * x)


  property("eval") = forAll(genArgN0) { (a: N0) =>
    op(a).eval[T0] shouldCloseTo math.acos(a.toStd)
  }

  property(s"${op("var0")} forward w.r.t self") = forAll(genV0(domain)) { (a: Var0) =>
    op(a).forward[N0, N0](a).eval[T0] shouldCloseTo deriv(a.toStd)
  }

  property(s"${op("var0")} reverse node0") = forAll(genV0(domain), genN0()) { (a: Var0, b: N0) =>
    val g = op(a).reverse(b)
    g(a).get.asInstanceOf[N0].eval[T0] shouldCloseTo b.toStd * deriv(a.toStd)
  }

  property(s"${op("var0")} reverse node1") = forAll(genV0(domain), genN1()) { (a: Var0, b: N1) =>
    val g = op(a).reverse(b)
    g(a).get.asInstanceOf[N1].eval[T1] shouldCloseTo (b.toStd mul deriv(a.toStd))
  }

  property(s"${op("var0")} reverse node2") = forAll(genV0(domain), genN2()) { (a: Var0, b: N2) =>
    val g = op(a).reverse(b)
    g(a).get.asInstanceOf[N2].eval[T2] shouldCloseTo (b.toStd mul deriv(a.toStd))
  }

}


