package com.kogecoo.scalaad.test.helper.impl.std

import com.kogecoo.scalaad.graph._
import com.kogecoo.scalaad.impl.std.Implicits._
import org.scalacheck.{Arbitrary, Gen}


object Implicits {

  private[this] type T0 = StdUtil.T0
  private[this] type T1 = StdUtil.T1
  private[this] type T2 = StdUtil.T2

  private[this] val stdN0LeafGen = new StdN0Gen()
  private[this] val stdN1LeafGen = new StdN1Gen()
  private[this] val stdN2LeafGen = new StdN2Gen()

  implicit class StdScalarOp(val self: T0) extends AnyVal {

    // ref: https://randomascii.wordpress.com/2012/02/25/comparing-floating-point-numbers-2012-edition/
    final def closeTo(rhs: T0, relDiff: T0 = 1e-5): Boolean = {
      math.abs(self - rhs) <= math.max(math.abs(self), math.abs(rhs)) * relDiff
    }

  }

  implicit class StdVecOp(val self: T1) extends AnyVal {

    final def broadcast(f: T0 => T0): T1 = self.map(f)

    final def elementwise(v: T1, f: (T0, T0) => T0): T1 = {
      self.zip(v).map { case (x, y) => f(x, y) }
    }

    final def equalTo(rhs: T1): Boolean = {
      if (StdUtil.shapeCheck1(self, rhs)) {
        self.zip(rhs).forall { case (l, r) => l == r }
      } else {
        throw new Exception(s"$self equalTo $rhs cannot calculate because its shapes are different.")
      }
    }

    final def closeTo(rhs: T1, relDiff: T0 = 1e-5): Boolean = {
      if (StdUtil.shapeCheck1(self, rhs)) {
        self.zip(rhs).forall { case (l, r) => l.closeTo(r, relDiff) }
      } else {
        throw new Exception(s"$self closeTo $rhs cannot calculate because its shapes are different.")
      }
    }

  }


  implicit class StdMatOp(val self: T2) extends AnyVal {

    final def broadcast(f: T0 => T0): T2 = self.map(_.map(f))

    final def elementwise(v: T2, f: (T0, T0) => T0): T2 = {
      self.zip(v).map { case (x, y) => x.zip(y).map { case (a, b) => f(a, b) } }
    }

    final def columnwise(rhs: T1, f: (T0, T0) => T0): T2 = {
      self.zip(rhs).map { case (x, y) => x.map(f(_, y)) }
    }

    final def rowwise(rhs: T1, f: (T0, T0) => T0): T2 = {
      self.map { _.zip(rhs).map { case (x, y) => f(x, y) } }
    }

    final def equalTo(rhs: T2): Boolean = {
      if (StdUtil.shapeCheck2(self, rhs)) {
        self.zip(rhs).forall { case (x, y) =>
          x.zip(y).forall { case (l, r) => l == r }
        }
      } else {
        throw new Exception(s"$self equalTo $rhs cannot calculate because its shapes are different.")
      }
    }

    final def closeTo(rhs: T2, relDiff: T0 = 1e-5): Boolean = {
      if (StdUtil.shapeCheck2(self, rhs)) {
        self.zip(rhs).forall { case (x, y) =>
          x.zip(y).forall { case (l, r) => l.closeTo(r, relDiff) }
        }
      } else {
        throw new Exception(s"$self closeTo $rhs cannot calculate because its shapes are different.")

      }
    }

  }


  implicit class N0Ops(val self: N0) extends AnyVal { def toT0: T0 = self.eval[T0] }

  implicit class N1Ops(val self: N1) extends AnyVal { def toT1: T1 = self.eval[T1] }

  implicit class N2Ops(val self: N2) extends AnyVal { def toT2: T2 = self.eval[T2] }

}
