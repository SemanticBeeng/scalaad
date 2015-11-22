package com.kogecoo.scalaad.test.node

import com.kogecoo.scalaad.graph._
import com.kogecoo.scalaad.impl.std.Implicits._
import com.kogecoo.scalaad.test.helper.impl.std.Implicits._
import com.kogecoo.scalaad.test.helper.impl.std.StdValueGen
import org.scalacheck.Prop.forAll
import org.scalacheck.{Gen, Properties}


object Acos1Spec extends Properties("Acos1") with NodeSpecBase {

  private[this] def deriv(x: Double): Double = -1.0 / math.sqrt(1.0 - x * x)

  override val defaultMinValue = Some(-1e10)
  override val defaultMaxValue = Some( 1e10)

  val domain = StdValueGen(
    Some(-1.0),
    Some(1.0),
    (x: Double) => x != -1.0 && x != 1.0
  )

  // exclude One1 node
  val genV1ForFunc: Gen[Var1] = genV1(value = domain)
  val genN1ForFunc: Gen[N1] = Gen.oneOf(genV1(value = domain), genConst1(value = domain), genHalf1(), genZero1())
  val genNV1ForFunc: Gen[N1] = Gen.oneOf(genConst1(value = domain), genHalf1(), genZero1())

  val genNV1ForFunc_N1: Gen[(N1, N1)] = {
    for {
      first  <- genNV1ForFunc
      second <- n1gen.genNode1(first.shape, genDefaultDomain)
    } yield (first, second)
  }

  val genNV1ForFunc_N2: Gen[(N1, N2)] = {
    for {
      first  <- genNV1ForFunc
      s2     =  genS2(first.shape._1)
      second <- n2gen.genNode2(s2, genDefaultDomain)
    } yield (first, second)
  }

  val genV1ForFunc_N1: Gen[(Var1, N1)] = {
    for {
      first  <- genV1ForFunc
      second <- n1gen.genNode1(first.shape, genDefaultDomain)
    } yield (first, second)
  }

  val genV1ForFunc_N2: Gen[(Var1, N2)] = {
    for {
      first  <- genV1ForFunc
      s2     =  genS2(first.shape._1)
      second <- n2gen.genNode2(s2, genDefaultDomain)
    } yield (first, second)
  }

  property("eval") = forAll(genN1ForFunc) { (a: N1) =>
    Acos1(a).eval[T1] shouldCloseTo a.toStd.map(math.acos)
  }

  property("acos(node1) forward w.r.t node0") = forAll(genN1ForFunc, genN0()) { (a: N1, b: N0) =>
    Acos1(a).forward[N0, N1](b).eval[T1] shouldCloseTo zero1(a)
  }

  property("acos(node1) forward w.r.t node1") = forAll(genN1ForFunc, genN1()) { (a: N1, b: N1) =>
    Acos1(a).forward[N1, N2](b).eval[T2] shouldCloseTo zero2(a, b)
  }

  property("acos(var1) forward w.r.t self") = forAll(genV1ForFunc) { (a: Var1) =>
    Acos1(a).forward[N1, N2](a).eval[T2] shouldCloseTo diag(a.toStd.map(deriv))
  }


  property("acos(nonvar1) reverse node0") = forAll(genNV1ForFunc, genN0()) { (a: N1, b: N0) =>
    Acos1(a).reverse(b).size == 0
  }

  property("acos(nonvar1) reverse node1") = forAll(genNV1ForFunc_N1) { case (a: N1, b: N1) =>
    Acos1(a).reverse(b).size == 0
  }

  property("acos(nonvar1) reverse node2") = forAll(genNV1ForFunc_N2) { case (a: N1, b: N2) =>
    Acos1(a).reverse(b).size == 0
  }

  property("acos(var1) reverse node0") = forAll(genV1ForFunc, genN0()) { (a: Var1, b: N0) =>
    val g = Acos1(a).reverse(b)
    g(a).get.asInstanceOf[N1].eval[T1] shouldCloseTo (a.toStd.map(deriv) mul b.toStd)
  }

  property("acos(var1) reverse node1") = forAll(genV1ForFunc_N1) { case (a: Var1, b: N1) =>
    val g = Acos1(a).reverse(b)
    g(a).get.asInstanceOf[N1].eval[T1] shouldCloseTo (a.toStd.map(deriv) mul b.toStd)
  }

  property("acos(var1) reverse node2") = forAll(genV1ForFunc_N2) { case (a: Var1, b: N2) =>
    val g = Acos1(a).reverse(b)
    g(a).get.asInstanceOf[N2].eval[T2] shouldCloseTo (b.toStd colMul a.toStd.map(deriv))
  }

}


