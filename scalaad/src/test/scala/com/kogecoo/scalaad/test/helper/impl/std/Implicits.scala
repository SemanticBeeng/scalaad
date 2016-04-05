package com.kogecoo.scalaad.test.helper.impl.std

import com.kogecoo.scalaad.graph._
import com.kogecoo.scalaad.impl.std.Implicits._
import com.kogecoo.scalaad.{StdMat, StdVec}
import org.scalacheck.{Arbitrary, Gen, Prop}
import org.scalacheck.Prop.BooleanOperators


object Implicits {

  type T0 = Double
  type T1 = StdVec[Double]
  type T2 = StdMat[Double]

  private[this] val stdN0LeafGen = new StdN0Gen()
  private[this] val stdN1LeafGen = new StdN1Gen()
  private[this] val stdN2LeafGen = new StdN2Gen()

  implicit class StdScalarOp(val self: T0) extends AnyVal {

    def shouldEqualTo(rhs: T0): Prop = {
      (self == rhs) :| s"$self \n  should equal to \n    $rhs"
    }

    // ref: https://randomascii.wordpress.com/2012/02/25/comparing-floating-point-numbers-2012-edition/
    def closeTo(rhs: T0, relDiff: T0 = 1e-5): Boolean = {
      math.abs(self - rhs) <= math.max(math.abs(self), math.abs(rhs)) * relDiff
    }

    def shouldCloseTo(rhs: T0, relDiff: T0 = 1e-5): Prop = {
      (self closeTo rhs) :| s"$self \n  should close to \n    $rhs"
    }
  }

  implicit class StdVecOp(val self: T1) extends AnyVal {

    def broadcast(f: T0 => T0): T1 = self.map(f)

    def elementwise(f: T0 => T0): T1 = self.map { x => f(x) }

    def elementwise(rhs: T1, f: (T0, T0) => T0): T1 = self.zip(rhs).map { case (x, y) => f(x, y) }

    private[this] def sizeCheck(rhs: T1): Boolean = self.size == rhs.size


    def equalTo(rhs: T1): Boolean = {
      sizeCheck(rhs) && self.zip(rhs).forall { case (l, r) => l == r }
    }

    def shouldEqualTo(rhs: T1, relDiff: T0 = 1e-5): Prop = {
      if (sizeCheck(rhs)) {
        (self equalTo rhs) :| s"$self \n  should equal to \n    $rhs"
      } else {
        false :| s"lhs length ${self.size} should equal to rhs length ${rhs.size}"
      }
    }

    def closeTo(rhs: T1, relDiff: T0 = 1e-5): Boolean = {
      sizeCheck(rhs) && self.zip(rhs).forall { case (l, r) => l.closeTo(r, relDiff) }
    }

    def shouldCloseTo(rhs: T1, relDiff: T0 = 1e-5): Prop = {
      if (sizeCheck(rhs)) {
        (self closeTo rhs) :| s"$self \n  should close to \n    $rhs"
      } else {
        false :| s"lhs length ${self.size} should equal to rhs length ${rhs.size}"
      }
    }

    def zero: T1         = broadcast(_ => 0.0)
    def one: T1          = broadcast(_ => 1.0)
    def const(v: T0): T1 = broadcast(_ => v)
    def neg: T1          = broadcast(-_)

    def add(rhs: T0): T1 = broadcast(_ + rhs)
    def add(rhs: T1): T1 = elementwise(rhs, (x, y) => x + y)

    def sub(rhs: T0): T1 = broadcast(_ - rhs)
    def sub(rhs: T1): T1 = elementwise(rhs, (x, y) => x - y)

    def mul(rhs: T0): T1 = broadcast(_ * rhs)
    def mul(rhs: T1): T1 = elementwise(rhs, (x, y) => x * y)

    def div(rhs: T0): T1 = broadcast(_ / rhs)
    def div(rhs: T1): T1 = elementwise(rhs, (x, y) => x / y)

    def dot(rhs: T1): T0 = (self mul rhs).sum
  }

  implicit class StdMatOp(val self: T2) extends AnyVal {

    def broadcast(f: T0 => T0): T2 = self.map(_.map(f))

    def elementwise(f: T0 => T0): T2 = self.map { x => x.map { a => f(a) } }

    def elementwise(rhs: T2, f: (T0, T0) => T0): T2 = {
      self.zip(rhs).map { case (x, y) => x.zip(y).map { case (a, b) => f(a, b) } }
    }

    private[this] def sizeCheck(rhs: T2): Boolean = self.size == rhs.size && self.head.size == rhs.head.size

    private[this] def columnWise(rhs: T1, f: (T0, T0) => T0): T2 = {
      self.zip(rhs).map { case (x, y) => x.map(f(_, y)) }
    }

    private[this] def rowWise(rhs: T1, f: (T0, T0) => T0): T2 = {
      self.map { _.zip(rhs).map { case (x, y) => f(x, y) } }
    }

    def equalTo(rhs: T2): Boolean = {
      sizeCheck(rhs) && self.zip(rhs).forall { case (x, y) =>
        x.zip(y).forall { case (l, r) => l == r }
      }
    }

    def shouldEqualTo(rhs: T2): Prop = {
      if (sizeCheck(rhs)) {
        (self equalTo rhs) :| s"$self \n  should equal to \n    $rhs"
      } else {
        false :| s"the shape of (${self.size}, ${self.head.size}) should equal to the shape of rhs(${rhs.size}, ${rhs.head.size})"
      }
    }

    def closeTo(rhs: T2, relDiff: T0 = 1e-5): Boolean = {
      sizeCheck(rhs) && self.zip(rhs).forall { case (x, y) =>
        x.zip(y).forall { case (l, r) => l.closeTo(r, relDiff) }
      }
    }

    def shouldCloseTo(rhs: T2, relDiff: T0 = 1e-5): Prop = {
      if (sizeCheck(rhs)) {
        (self closeTo rhs) :| s"$self \n  should close to \n    $rhs"
      } else {
        false :| s"the shape of (${self.size}, ${self.head.size}) should equal to the shape of rhs(${rhs.size}, ${rhs.head.size})"
      }
    }

    def zero: T2         = broadcast(_ => 0.0)
    def one: T2          = broadcast(_ => 1.0)
    def const(v: T0): T2 = broadcast(_ => v)
    def neg: T2          = broadcast(-_)

    def add(rhs: T2): T2 = elementwise(rhs, (x, y) => x + y)

    def sub(rhs: T2): T2 = elementwise(rhs, (x, y) => x - y)

    def mul(x: T0, y: T0): T0 = x * y
    def mul(rhs: T2): T2      = elementwise(rhs, mul)
    def mul(rhs: T0): T2      = broadcast(_ * rhs)
    def colMul(rhs: T1): T2   = columnWise(rhs, mul)
    def rowMul(rhs: T1): T2   = rowWise(rhs, mul)

    def div(x: T0, y: T0): T0 = x / y
    def div(rhs: T2): T2 = elementwise(rhs, div)
    def div(rhs: T0): T2 = broadcast(_ / rhs)
    def colDiv(rhs: T1): T2   = columnWise(rhs, div)
    def rowDiv(rhs: T1): T2   = rowWise(rhs, div)

    def matmul(rhs: T2): T2 = {
      //assert(a.head.size == b.size)
      (0 until rhs.shape._2).map { rcolIndex =>
        self.map { lrow =>
          lrow.zip(rhs.map(_.apply(rcolIndex))).map({ case (x, y) => x * y }).sum
        }
      }
    }

  }


  implicit class N0Ops(val self: N0) extends AnyVal { def toStd: T0 = self.eval[T0] }

  implicit class N1Ops(val self: N1) extends AnyVal { def toStd: T1 = self.eval[T1] }

  implicit class N2Ops(val self: N2) extends AnyVal { def toStd: T2 = self.eval[T2] }

  implicit def arbStdVar0(implicit vg: Gen[T0]): Arbitrary[Var0] = Arbitrary {
    stdN0LeafGen.genVar0(vg)
  }

  implicit def arbStdNode0(implicit vg: Gen[T0]): Arbitrary[N0] = Arbitrary {
    stdN0LeafGen.genNode0(vg)
  }

  implicit def arbStdVar1(implicit sg: Gen[S1], vg: Gen[T0]): Arbitrary[Var1] = Arbitrary {
    stdN1LeafGen.genVar1(sg, vg)
  }

  implicit def arbStdNode1(implicit sg: Gen[S1], vg: Gen[T0]): Arbitrary[N1] = Arbitrary {
    stdN1LeafGen.genNode1(sg, vg)
  }

  implicit def arbStdVar2(implicit sg: Gen[S2], vg: Gen[T0]): Arbitrary[Var2] = Arbitrary {
    stdN2LeafGen.genVar2(sg, vg)
  }

  implicit def arbStdNode2(implicit sg: Gen[S2], vg: Gen[T0]): Arbitrary[N2] = Arbitrary {
    stdN2LeafGen.genNode2(sg, vg)
  }

}
