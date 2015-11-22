package com.kogecoo.scalaad.test.node

import com.kogecoo.scalaad.graph._
import com.kogecoo.scalaad.impl.std.Implicits._
import com.kogecoo.scalaad.test.helper._
import com.kogecoo.scalaad.test.helper.impl.std.Implicits._
import com.kogecoo.scalaad.test.helper.impl.std._
import com.kogecoo.scalaad.{Shape2, StdMat, StdVec}
import org.scalacheck.Prop.forAll
import org.scalacheck.{Gen, Properties}


/*
object MatMul22Spec extends Properties("MatMul22") {

  type T = Double
  type T2 = StdMat[T]

  type V = Var2Leaf[T]
  type L0 = N0Leaf[T]
  type L1 = N1Leaf[StdVec[T]]
  type L2 = N2Leaf[T2]

  val consForValue = new LeafConstraint[Double]((x: Double) => x > -1e100 && x < 1e100)
  val consForS1 = new S1Constraint((_: S1) => true, l => l > 1 && l < 100)
  val consForS2 = new S2Constraint((_: S2) => true, _ => true, _ => true)

  val v0 = new StdN0LeafGen().genVar0(consForValue)
  val n0 = new StdN0LeafGen().genNode0(consForValue)
  val n1 = new StdN1LeafGen().genNode1(consForS1, consForValue)
  val n2 = new StdN2LeafGen().genNode2(consForS2, consForValue)
  val nv0 = new StdN0LeafGen().genNonVar0(consForValue)

  val n0Gen = new StdN0LeafGen()
  val n2Gen = new StdN2LeafGen()

  private[this] def genL2L2(shapeCons: S2Constraint = consForS2, valueCons: LeafConstraint[Double] = consForValue): Gen[(N2Leaf[T2], N2Leaf[T2])] = {
    for {
      first  <- n2Gen.genNode2(shapeCons, valueCons)
      second <- n2Gen.genNode2(first.node.shape._1, shapeCons.colCons, shapeCons.shapeCons, valueCons)
    } yield (first, second)
  }

  private[this] def genNonVar2NonVar2ReverseL2(shapeCons: S2Constraint = consForS2, valueCons: LeafConstraint[Double] = consForValue): Gen[(N2Leaf[T2], N2Leaf[T2], N2Leaf[T2])] = {
    for {
      first   <- n2Gen.genNonVar2(shapeCons, valueCons)
      second  <- n2Gen.genNonVar2(first.node.shape._2, shapeCons.colCons, shapeCons.shapeCons, valueCons)
      reverse <- n2Gen.genNonVar2(second.node.shape._1, first.node.shape._2, shapeCons.shapeCons, valueCons)
    } yield (first, second, reverse)
  }

  private[this] def genVar2NonVar2ReverseL2(shapeCons: S2Constraint = consForS2, valueCons: LeafConstraint[Double] = consForValue): Gen[(Var2Leaf[T2], N2Leaf[T2], N2Leaf[T2])] = {
    for {
      first   <- n2Gen.genVar2(shapeCons, valueCons)
      second  <- n2Gen.genNonVar2(first.node.shape._2, shapeCons.colCons, shapeCons.shapeCons, valueCons)
      reverse <- n2Gen.genNonVar2(second.node.shape._1, first.node.shape._2, shapeCons.shapeCons, valueCons)
    } yield (first, second, reverse)
  }

  private[this] def genNonVar2Var2ReverseL2(shapeCons: S2Constraint = consForS2, valueCons: LeafConstraint[Double] = consForValue): Gen[(N2Leaf[T2], Var2Leaf[T2], N2Leaf[T2])] = {
    for {
      first   <- n2Gen.genNonVar2(shapeCons, valueCons)
      second  <- n2Gen.genVar2(first.node.shape._2, shapeCons.colCons, shapeCons.shapeCons, valueCons)
      reverse <- n2Gen.genNonVar2(second.node.shape._1, first.node.shape._2, shapeCons.shapeCons, valueCons)
    } yield (first, second, reverse)
  }

  private[this] def genVar2Var2ReverseL2(shapeCons: S2Constraint = consForS2, valueCons: LeafConstraint[Double] = consForValue): Gen[(Var2Leaf[T2], Var2Leaf[T2], N2Leaf[T2])] = {
    for {
      first   <- n2Gen.genVar2(shapeCons, valueCons)
      second  <- n2Gen.genVar2(first.node.shape._2, shapeCons.colCons, shapeCons.shapeCons, valueCons)
      reverse <- n2Gen.genNonVar2(second.node.shape._1, first.node.shape._2, shapeCons.shapeCons, valueCons)
    } yield (first, second, reverse)
  }

  def stdMatMul(a: T2, b: T2): T2 = {
    //assert(a(0).size == b.size)
    (0 until b.shape._2).map { bcolIndex =>
      a.map { arow =>
        arow.zip(b.map(_.apply(bcolIndex))).map({ case (x, y) => x * y }).sum
      }
    }
  }

  property("eval") = forAll(genL2L2()) { case (a: L2, b: L2) =>
    val expect = stdMatMul(a.toStd, b.toStd)
    MatMul22(a.node, b.node).eval[T2] shouldBeCloseTo expect
  }

  property("MatMul(node2, node2) forward w.r.t node0") = {
    forAll(genL2L2(), n0Gen.genNode0()) { case ((a, b), c) =>
      val expect = Zero2(new Shape2(a.node.shape._1, b.node.shape._2)).eval[T2]
      MatMul22(a.node, b.node).forward[N0, N2](c.node).eval[T2] shouldBeCloseTo expect
    }
  }

  property("MatMul(nonvar2, nonvar2) reverse node2") = {
    forAll(genNonVar2NonVar2ReverseL2()) { case (a, b, c) =>
      MatMul22(a.node, b.node).reverse(c.node).size == 0
    }
  }
  property("MatMul(var2, nonvar2) reverse node2") = {
    forAll(genVar2NonVar2ReverseL2()) { case (a, b, c) =>
      val l = stdMatMul(c.node.eval[T2], b.node.eval[T2])
      val r = stdMatMul(a.node.eval[T2], c.node.eval[T2])
      val expect = l.zip(r).map { case (x, y) => x.zip(y).map { case (i, j) => i + j } }

      val g = MatMul22(a.node, b.node).reverse(c.node)
      g(a.node).get.asInstanceOf[N2].eval[T2] shouldBeCloseTo expect
    }
  }

  property("MatMul(nonvar2, var2) reverse node2") = {
    forAll(genNonVar2Var2ReverseL2()) { case (a, b, c) =>
      val expect = stdMatMul(a.node.eval[T2], c.node.eval[T2])

      val g = MatMul22(a.node, b.node).reverse(c.node)
      g(b.node).get.asInstanceOf[N2].eval[T2] shouldBeCloseTo expect
    }
  }

  property("MatMul(var2, var2) reverse node2") = {
    forAll(genVar2Var2ReverseL2()) { case (a, b, c) =>
      val g = MatMul22(a.node, b.node).reverse(c.node)
      g(a.node).get.asInstanceOf[N2].eval[T2] shouldBeCloseTo c.toStd.map(_.map(_ * 2))
    }
  }
}

*/
