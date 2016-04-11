package com.kogecoo.scalaad.test.node

import com.kogecoo.scalaad.graph._
import com.kogecoo.scalaad.test.helper.impl.std.Implicits._
import com.kogecoo.scalaad.test.helper.impl.std._
import com.kogecoo.scalaad.{Shape2, StdMat, StdVec}
import org.scalacheck.Gen


trait NodeSpecBase {

  type T0  = Double
  type T1  = StdVec[T0]
  type T2  = StdMat[T0]

  def defaultMinValue: Option[T0] = None
  def defaultMaxValue: Option[T0] = None
  def defaultValueConstraint: T0 => Boolean = _ => true

  def defaultS1SizeConstraint: Int => Boolean = _ => true
  def defaultS1SizeMin: Int = 1
  def defaultS1SizeMax: Int = 10
  def defaultS1ShapeConstraint: S1 => Boolean = _ => true

  def defaultS2RowConstraint: Int => Boolean = _ => true
  def defaultS2RowMin: Int = 1
  def defaultS2RowMax: Int = 10
  def defaultS2ColConstraint: Int => Boolean = _ => true
  def defaultS2ColMin: Int = 1
  def defaultS2ColMax: Int = 10
  def defaultS2ShapeConstraint: S2 => Boolean = _ => true

  final def eye(forRow: N1, forCol: N1): T2 = eye(Shape2(forRow, forCol))
  final def eye(l1: N1): T2 = eye(l1, l1)
  final def eye(shape: S2): T2 = {
    (0 until shape._1).map { i =>
      (0 until shape._2).map { j => if (i == j) 1.0 else 0.0 }
    }
  }

  final def diag(v: T1): T2 = {
    v.indices.map { i =>
      v.indices.map { j => if (i == j) v(i) else 0.0 }
    }
  }

  final def genDefaultDomain = StdValueGen(defaultMinValue, defaultMaxValue, defaultValueConstraint)
  final def genS1() = S1Gen(defaultS1SizeConstraint, defaultS1ShapeConstraint, defaultS1SizeMin, defaultS1SizeMax)
  final def genS1(size: Int) = S1Gen(size, defaultS1ShapeConstraint)
  final def genS2() = S2Gen(
    defaultS2RowConstraint,
    defaultS2ColConstraint,
    defaultS2ShapeConstraint,
    defaultS2RowMin,
    defaultS2RowMax,
    defaultS2ColMin,
    defaultS2ColMax
  )
  final def genS2(row: Int) = S2Gen(
    row,
    defaultS2ColConstraint,
    defaultS2ShapeConstraint,
    defaultS2ColMin,
    defaultS2ColMax
  )
  final def genS2(shape: S1): Gen[S2] = genS2(shape._1)

  // should move to std implicit
  final def const1(v: T0, l1: N1): T1 = l1.toStd.const(v)
  final def const1(v: T0, s1: S1): T1 = Seq.fill[T0](s1._1)(v)
  final def const2(v: T0, l2: N2): T2 = l2.toStd.const(v)
  final def const2(v: T0, s2: S2): T2 = Seq.fill[T0](s2._1, s2._2)(v)

  final def one1(l1: N1): T1 = l1.toStd.one
  final def one1(s1: S1): T1 = Seq.fill[Double](s1._1)(1.0)
  final def one2(l2: N2): T2 = l2.toStd.one
  final def one2(s2: S2): T2 = Seq.fill[Double](s2._1, s2._2)(1.0)

  final def zero1(l1: N1): T1 = l1.toStd.zero
  final def zero1(s1: S1): T1 = Seq.fill[Double](s1._1)(0.0)
  final def zero2(l2: N2): T2 = l2.toStd.zero
  final def zero2(s2: S2): T2 = Seq.fill[Double](s2._1, s2._2)(0.0)
  final def zero2(refRowSize: N1, refColSize: N1): T2 = zero2(Shape2(refRowSize.shape._1, refColSize.shape._1))

  // shorthands

  final val n0gen = new StdN0Gen()
  final val n1gen = new StdN1Gen()
  final val n2gen = new StdN2Gen()

  final def genConst0(                        value: Gen[T0] = genDefaultDomain) = n0gen.genConst0(       value)
  final def genConst1(shape: Gen[S1] = genS1(), value: Gen[T0] = genDefaultDomain) = n1gen.genConst1(shape, value)
  final def genConst2(shape: Gen[S2] = genS2(), value: Gen[T0] = genDefaultDomain) = n2gen.genConst2(shape, value)

  final def genHalf0()                       = n0gen.genHalf0()
  final def genHalf1(shape: Gen[S1] = genS1()) = n1gen.genHalf1(shape)
  final def genHalf2(shape: Gen[S2] = genS2()) = n2gen.genHalf2(shape)

  final def genOne0()                       = n0gen.genOne0()
  final def genOne1(shape: Gen[S1] = genS1()) = n1gen.genOne1(shape)
  final def genOne2(shape: Gen[S2] = genS2()) = n2gen.genOne2(shape)

  final def genZero0()                       = n0gen.genZero0()
  final def genZero1(shape: Gen[S1] = genS1()) = n1gen.genZero1(shape)
  final def genZero2(shape: Gen[S2] = genS2()) = n2gen.genZero2(shape)

  final def genV0(                        value: Gen[T0] = genDefaultDomain) = n0gen.genVar0(       value)
  final def genV1(shape: Gen[S1] = genS1(), value: Gen[T0] = genDefaultDomain) = n1gen.genVar1(shape, value)
  final def genV2(shape: Gen[S2] = genS2(), value: Gen[T0] = genDefaultDomain) = n2gen.genVar2(shape, value)

  final def genN0(                        value: Gen[T0] = genDefaultDomain) = n0gen.genNode0(       value)
  final def genN1(shape: Gen[S1] = genS1(), value: Gen[T0] = genDefaultDomain) = n1gen.genNode1(shape, value)
  final def genN2(shape: Gen[S2] = genS2(), value: Gen[T0] = genDefaultDomain) = n2gen.genNode2(shape, value)

  final def genNV0(                        value: Gen[T0] = genDefaultDomain) = n0gen.genNonVar0(       value)
  final def genNV1(shape: Gen[S1] = genS1(), value: Gen[T0] = genDefaultDomain) = n1gen.genNonVar1(shape, value)
  final def genNV2(shape: Gen[S2] = genS2(), value: Gen[T0] = genDefaultDomain) = n2gen.genNonVar2(shape, value)

  final def genNonzeroN0(                        value: Gen[T0] = genDefaultDomain) = n0gen.genNonzeroNode0(value)
  final def genNonzeroN1(shape: Gen[S1] = genS1(), value: Gen[T0] = genDefaultDomain) = n1gen.genNonzeroNode1(shape, value)
  final def genNonzeroN2(shape: Gen[S2] = genS2(), value: Gen[T0] = genDefaultDomain) = n2gen.genNonzeroNode2(shape, value)

  final def genNonzeroNV0(                        value: Gen[T0] = genDefaultDomain) = n0gen.genNonzeroNonVar0(value)
  final def genNonzeroNV1(shape: Gen[S1] = genS1(), value: Gen[T0] = genDefaultDomain) = n1gen.genNonzeroNonVar1(shape, value)
  final def genNonzeroNV2(shape: Gen[S2] = genS2(), value: Gen[T0] = genDefaultDomain) = n2gen.genNonzeroNonVar2(shape, value)

  final def genN1_N1(
      domain1: Gen[T0] = genDefaultDomain,
      domain2: Gen[T0] = genDefaultDomain
  ): Gen[(N1, N1)] = {
    for {
      first  <- n1gen.genNode1(genS1(), domain1)
      second <- n1gen.genNode1(first.shape, domain2)
    } yield (first, second)
  }

  final def genNV1_NV1(
      domain1: Gen[T0] = genDefaultDomain,
      domain2: Gen[T0] = genDefaultDomain
  ): Gen[(N1, N1)] = {
    for {
      first  <- n1gen.genNonVar1(genS1(), domain1)
      second <- n1gen.genNonVar1(first.shape, domain2)
    } yield (first, second)
  }

  final def genNV1_N1(
      domain1: Gen[T0] = genDefaultDomain,
      domain2: Gen[T0] = genDefaultDomain
  ): Gen[(N1, N1)] = {
    for {
      first  <- n1gen.genNonVar1(genS1(), domain1)
      second <- n1gen.genNode1(first.shape, domain2)
    } yield (first, second)
  }

  final def genV1_N1(
      domain1: Gen[T0] = genDefaultDomain,
      domain2: Gen[T0] = genDefaultDomain
  ): Gen[(N1, N1)] = {
    for {
      first  <- n1gen.genVar1(genS1(), domain1)
      second <- n1gen.genNode1(first.shape, domain2)
    } yield (first, second)
  }

  final def genV1_RowEquivN2(
      domain1: Gen[T0] = genDefaultDomain,
      domain2: Gen[T0] = genDefaultDomain
  ): Gen[(N1, N2)] = {
    for {
      first  <- n1gen.genVar1(genS1(), domain1)
      second <- n2gen.genNode2(genS2(first.shape._1), domain2)
    } yield (first, second)
  }

  final def genNV1_RowEquivN2(
      domain1: Gen[T0] = genDefaultDomain,
      domain2: Gen[T0] = genDefaultDomain
  ): Gen[(N1, N2)] = {
    for {
      first  <- n1gen.genNonVar1(genS1(), domain1)
      second <- n2gen.genNode2(genS2(first.shape._1), domain2)
    } yield (first, second)
  }

  final def genV1_NV1(
      domain1: Gen[T0] = genDefaultDomain,
      domain2: Gen[T0] = genDefaultDomain
  ): Gen[(N1, N1)] = {
    for {
      first  <- n1gen.genVar1(genS1(), domain1)
      second <- n1gen.genNonVar1(first.shape, domain2)
    } yield (first, second)
  }

  final def genN1_V1(
      domain1: Gen[T0] = genDefaultDomain,
      domain2: Gen[T0] = genDefaultDomain
  ): Gen[(N1, N1)] = {
    for {
      first  <- n1gen.genNode1(genS1(), domain1)
      second <- n1gen.genVar1(first.shape, domain2)
    } yield (first, second)
  }

  final def genNV1_V1(
      domain1: Gen[T0] = genDefaultDomain,
      domain2: Gen[T0] = genDefaultDomain
  ): Gen[(N1, N1)] = {
    for {
      first  <- n1gen.genNonVar1(genS1(), domain1)
      second <- n1gen.genVar1(first.shape, domain2)
    } yield (first, second)
  }

  final def genV1_NV1_N1(
      domain1: Gen[T0] = genDefaultDomain,
      domain2: Gen[T0] = genDefaultDomain,
      domain3: Gen[T0] = genDefaultDomain
  ): Gen[(Var1, N1, N1)] = {
    for {
      first   <- n1gen.genVar1(genS1(), domain1)
      second  <- n1gen.genNonVar1(first.shape, domain2)
      reverse <- n1gen.genNode1(first.shape, domain3)
    } yield (first, second, reverse)
  }

  final def genNV1_V1_N1(
    domain1: Gen[T0] = genDefaultDomain,
    domain2: Gen[T0] = genDefaultDomain,
    domain3: Gen[T0] = genDefaultDomain
  ): Gen[(N1, Var1, N1)] = {
    for {
      first   <- n1gen.genNonVar1(genS1(), domain1)
      second  <- n1gen.genVar1(first.shape, domain2)
      reverse <- n1gen.genNode1(second.shape, domain3)
    } yield (first, second, reverse)
  }

  final def genV1_V1_N1(
    domain1: Gen[T0] = genDefaultDomain,
    domain2: Gen[T0] = genDefaultDomain,
    domain3: Gen[T0] = genDefaultDomain
  ): Gen[(Var1, Var1, N1)] = {
    for {
      first   <- n1gen.genVar1(genS1(), domain1)
      second  <- n1gen.genVar1(first.shape, domain2)
      reverse <- n1gen.genNode1(second.shape, domain3)
    } yield (first, second, reverse)
  }

  final def genNV1_NV1_N1(
    domain1: Gen[T0] = genDefaultDomain,
    domain2: Gen[T0] = genDefaultDomain,
    domain3: Gen[T0] = genDefaultDomain
  ): Gen[(N1, N1, N1)] = {
    for {
      first   <- n1gen.genNonVar1(genS1(), domain1)
      second  <- n1gen.genNonVar1(first.shape, domain2)
      reverse <- n1gen.genNode1(second.shape, domain3)
    } yield (first, second, reverse)
  }

  final def genN1_N1_N1(
    domain1: Gen[T0] = genDefaultDomain,
    domain2: Gen[T0] = genDefaultDomain,
    domain3: Gen[T0] = genDefaultDomain
  ): Gen[(N1, N1, N1)] = {
    for {
      first   <- n1gen.genNode1(genS1(), domain1)
      second  <- n1gen.genNode1(first.shape, domain2)
      reverse <- n1gen.genNode1(second.shape, domain3)
    } yield (first, second, reverse)
  }

  final def genNV1_NV1_RowEquivN2(
    domain1: Gen[T0] = genDefaultDomain,
    domain2: Gen[T0] = genDefaultDomain,
    domain3: Gen[T0] = genDefaultDomain
  ): Gen[(N1, N1, N2)] = {
    for {
      first   <- n1gen.genNonVar1(genS1(), domain1)
      second  <- n1gen.genNonVar1(first.shape, domain2)
      reverse <- n2gen.genNode2(genS2(first.shape._1), domain3)
    } yield (first, second, reverse)
  }

  final def genV1_NV1_RowEquivN2(
    domain1: Gen[T0] = genDefaultDomain,
    domain2: Gen[T0] = genDefaultDomain,
    domain3: Gen[T0] = genDefaultDomain
  ): Gen[(Var1, N1, N2)] = {
    for {
      first   <- n1gen.genVar1(genS1(), domain1)
      second  <- n1gen.genNonVar1(first.shape, domain2)
      reverse <- n2gen.genNode2(genS2(first.shape._1), domain3)
    } yield (first, second, reverse)
  }

  final def genNV1_V1_RowEquivN2(
    domain1: Gen[T0] = genDefaultDomain,
    domain2: Gen[T0] = genDefaultDomain,
    domain3: Gen[T0] = genDefaultDomain
  ): Gen[(N1, Var1, N2)] = {
    for {
      first   <- n1gen.genNonVar1(genS1(), domain1)
      second  <- n1gen.genVar1(first.shape, domain2)
      reverse <- n2gen.genNode2(genS2(first.shape._1), domain3)
    } yield (first, second, reverse)
  }

  final def genV1_V1_RowEquivN2(
    domain1: Gen[T0] = genDefaultDomain,
    domain2: Gen[T0] = genDefaultDomain,
    domain3: Gen[T0] = genDefaultDomain
  ): Gen[(Var1, Var1, N2)] = {
    for {
      first   <- n1gen.genVar1(genS1(), domain1)
      second  <- n1gen.genVar1(first.shape, domain2)
      reverse <- n2gen.genNode2(genS2(first.shape._1), domain3)
    } yield (first, second, reverse)
  }

}


