package com.kogecoo.scalaad.test.node

import com.kogecoo.scalaad.graph._
import com.kogecoo.scalaad.impl.std.Implicits._
import com.kogecoo.scalaad.test.helper.impl.std.Implicits._
import com.kogecoo.scalaad.test.helper.impl.std.StdValueGen
import org.scalacheck.Prop.forAll
import org.scalacheck.{Gen, Properties}


object Acos0Spec extends Properties("Acos0") with NodeSpecBase {

  private[this] def deriv(x: Double): Double = -1.0 / math.sqrt(1.0 - x * x)

  override val defaultMinValue = Some(-1e10)
  override val defaultMaxValue = Some( 1e10)

  val domain = StdValueGen(
    Some(-1.0),
    Some(1.0),
    (x: Double) => x != -1.0 && x != 1.0
  )

  // exclude One0 node
  val genN0ForFunc: Gen[N0] = Gen.oneOf(
    genV0(domain),
    genConst0(domain),
    genHalf0(),
    genZero0()
  )

  val genNV0ForFunc: Gen[N0]  = Gen.oneOf(
    genConst0(domain),
    genHalf0(),
    genZero0()
  )

  property("eval") = forAll(genN0ForFunc) { (a: N0) =>
    Acos0(a).eval[T0] shouldCloseTo math.acos(a.toStd)
  }

  property("acos(node0) forward w.r.t node0") = forAll(genN0ForFunc, genN0()) { (a: N0, b: N0) =>
    Acos0(a).forward[N0, N0](b).eval[T0] shouldCloseTo 0.0
  }

  property("acos(node0) forward w.r.t node1") = forAll(genN0ForFunc, genN1()) { (a: N0, b: N1) =>
    Acos0(a).forward[N1, N1](b).eval[T1] shouldCloseTo zero1(b)
  }

  property("acos(node0) forward w.r.t node2") = forAll(genN0ForFunc, genN2()) { (a: N0, b: N2) =>
    Acos0(a).forward[N2, N2](b).eval[T2] shouldCloseTo zero2(b)
  }

  property("acos(var0) forward w.r.t self") = forAll(genV0(domain)) { (a: Var0) =>
    Acos0(a).forward[N0, N0](a).eval[T0] shouldCloseTo deriv(a.toStd)
  }


  property("acos(nonvar0) reverse node0") = forAll(genNV0ForFunc, genN0()) { (a: N0, b: N0) =>
    Acos0(a).reverse(b).size == 0
  }

  property("acos(nonvar0) reverse node1") = forAll(genNV0ForFunc, genN1()) { (a: N0, b: N1) =>
    Acos0(a).reverse(b).size == 0
  }

  property("acos(nonvar0) reverse node2") = forAll(genNV0ForFunc, genN2()) { (a: N0, b: N2) =>
    Acos0(a).reverse(b).size == 0
  }

  property("acos(var0) reverse node0") = forAll(genV0(domain), genN0()) { (a: Var0, b: N0) =>
    val g = Acos0(a).reverse(b)
    g(a).get.asInstanceOf[N0].eval[T0] shouldCloseTo b.toStd * deriv(a.toStd)
  }

  property("acos(var0) reverse node1") = forAll(genV0(domain), genN1()) { (a: Var0, b: N1) =>
    val g = Acos0(a).reverse(b)
    g(a).get.asInstanceOf[N1].eval[T1] shouldCloseTo (b.toStd mul deriv(a.toStd))
  }

  property("acos(var0) reverse node2") = forAll(genV0(domain), genN2()) { (a: Var0, b: N2) =>
    val g = Acos0(a).reverse(b)
    g(a).get.asInstanceOf[N2].eval[T2] shouldCloseTo (b.toStd mul deriv(a.toStd))
  }

}

