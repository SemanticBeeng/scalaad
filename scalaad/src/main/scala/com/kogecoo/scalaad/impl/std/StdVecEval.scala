package com.kogecoo.scalaad.impl.std

import com.kogecoo.scalaad.StdVec
import com.kogecoo.scalaad.algorithm.Eval
import com.kogecoo.scalaad.graph._
import com.kogecoo.scalaad.impl.std.Implicits._


trait StdVecEval {

  private[this] type V1[T] = StdVec[T]

  private[this] def fill[T](value: T, shape: S1): V1[T] = Seq.fill(shape._1)(value)

  private[this] def map1[T](v: V1[T], f: T => T): V1[T] = v.map(f(_))
  private[this] def map01[T](l: T    , r: V1[T], f: (T, T) => T): V1[T] = r.map(f(l, _))
  private[this] def map10[T](l: V1[T], r: T    , f: (T, T) => T): V1[T] = l.map(f(_, r))
  private[this] def map11[T](l: V1[T], r: V1[T], f: (T, T) => T): V1[T] = l.zip(r).map { case (x, y) => f(x, y) }

  private[this] def pos(v: Double): Double = +v
  private[this] def neg(v: Double): Double = -v
  private[this] def add(l: Double, r: Double): Double = l + r
  private[this] def sub(l: Double, r: Double): Double = l - r
  private[this] def mul(l: Double, r: Double): Double = l * r
  private[this] def div(l: Double, r: Double): Double = l / r

  implicit val eval11_stdvec_double: Eval[N1, StdVec[Double]] = new Eval[N1, StdVec[Double]] {

    private[this] type T = Double
    private[this] type V = StdVec[T]
    private[this] type N = N1

    def eval(n: N): V = n match {

      // Leaf nodes
      case Var1(v, _)                 => v.value[V]
      case ArbVar1(name, data, shape) => data.get.value[V]
      case Zero1(shape: S1)           => fill(0.0, shape)
      case Half1(shape: S1)           => fill(0.5, shape)
      case One1(shape: S1)            => fill(1.0, shape)
      case Const1(v, shape)           => v.value[V]

      // Unary ops
      case Pos1(v: N) => map1(v.eval[V], pos)
      case Neg1(v: N) => map1(v.eval[V], neg)

      // Binary ops
      case Add01(l: N0, r: N)  => map01[T](l.eval[T], r.eval[V], add)
      case Add10(l: N , r: N0) => map10[T](l.eval[V], r.eval[T], add)
      case Add11(l: N , r: N)  => map11[T](l.eval[V], r.eval[V], add)

      case Sub01(l: N0, r: N)  => map01[T](l.eval[T], r.eval[V], sub)
      case Sub10(l: N , r: N0) => map10[T](l.eval[V], r.eval[T], sub)
      case Sub11(l: N , r: N)  => map11[T](l.eval[V], r.eval[V], sub)

      case Mul01(l: N0, r: N)  => map01[T](l.eval[T], r.eval[V], mul)
      case Mul10(l: N , r: N0) => map10[T](l.eval[V], r.eval[T], mul)
      case Mul11(l: N , r: N)  => map11[T](l.eval[V], r.eval[V], mul)

      case Div01(l: N0, r: N)  => map01[T](l.eval[T], r.eval[V], div)
      case Div10(l: N , r: N0) => map10[T](l.eval[V], r.eval[T], div)
      case Div11(l: N , r: N)  => map11[T](l.eval[V], r.eval[V], div)

      // Math
      case Sin1(v: N) => map1(v.eval[V], math.sin)
      case Cos1(v: N) => map1(v.eval[V], math.cos)
      case Tan1(v: N) => map1(v.eval[V], math.tan)

      case Asin1(v: N) => map1(v.eval[V], math.asin)
      case Acos1(v: N) => map1(v.eval[V], math.acos)
      case Atan1(v: N) => map1(v.eval[V], math.atan)

      case Sinh1(v: N) => map1(v.eval[V], math.sinh)
      case Cosh1(v: N) => map1(v.eval[V], math.cosh)
      case Tanh1(v: N) => map1(v.eval[V], math.tanh)

      case Ln1(v: N)   => map1(v.eval[V], math.log)
      case Exp1(v: N)  => map1(v.eval[V], math.exp)
      case Sqrt1(v: N) => map1(v.eval[V], math.sqrt)

      case Pow01(l: N0, r: N)  => map01[T](l.eval[T], r.eval[V], math.pow)
      case Pow10(l: N , r: N0) => map10[T](l.eval[V], r.eval[T], math.pow)
      case Pow11(l: N , r: N)  => map11[T](l.eval[V], r.eval[V], math.pow)

      case Abs1(v: N)          => map1[T](v.eval[V], math.abs)
      case Max01(l: N0, r: N)  => map01[T](l.eval[T], r.eval[V], math.max)
      case Max10(l: N , r: N0) => map10[T](l.eval[V], r.eval[T], math.max)
      case Max11(l: N , r: N)  => map11[T](l.eval[V], r.eval[V], math.max)
      case Min01(l: N0, r: N)  => map01[T](l.eval[T], r.eval[V], math.min)
      case Min10(l: N , r: N0) => map10[T](l.eval[V], r.eval[T], math.min)
      case Min11(l: N , r: N)  => map11[T](l.eval[V], r.eval[V], math.min)

      case Where0_1(cond: B0, a: N, b: N) => if (cond.eval[Boolean]) a.eval[V] else b.eval[V]
      case Where1_1(cond: B1, a: N, b: N) => {
        val ab = a.eval[V].zip(b.eval[V])
        cond.eval[StdVec[Boolean]].zip(ab).map { case (c, (x, y)) => if (c) x else y }
      }
    }
  }

  // Experimental
  /*implicit val eval10_stdvec_dobule: Eval[N1, Double] = new Eval[N1, Double] {

    private[this] type T = Double
    private[this] type V = StdVec[T]

    def eval(n: N1): T = n match {
      case Max1(v)   => v.eval[V].max
      case Min1(v)   => v.eval[V].min
      case L0Norm(v) => v.eval[V].count(_ > 0.0)
      case L1Norm(v) => v.eval[V].map(math.abs).sum
      case L2Norm(v) => math.sqrt(v.eval[V].map(a => a * a).sum)
    }

  }*/

  implicit val eval_bool11_stdvec_double: Eval[B1, StdVec[Boolean]] = new Eval[B1, StdVec[Boolean]] {

    private[this] def map01(l: N0, r: N1, f: (Double, Double) => Boolean): StdVec[Boolean] = {
      val left = l.eval[Double]
      val right = r.eval[StdVec[Double]]
      right.map(f(left, _))
    }

    private[this] def map10(l: N1, r: N0, f: (Double, Double) => Boolean): StdVec[Boolean] = {
      val left = l.eval[StdVec[Double]]
      val right = r.eval[Double]
      left.map(f(_, right))
    }
    private[this] def map11(l: N1, r: N1, f: (Double, Double) => Boolean): StdVec[Boolean] = {
      l.eval[StdVec[Double]].zip(r.eval[StdVec[Double]]).map { case (a, b) => f(a, b) }
    }

    private[this] def mapb01(l: B0, r: B1, f: (Boolean, Boolean) => Boolean): StdVec[Boolean] = {
      val left = l.eval[Boolean]
      val right = r.eval[StdVec[Boolean]]
      right.map(f(left, _))
    }

    private[this] def mapb10(l: B1, r: B0, f: (Boolean, Boolean) => Boolean): StdVec[Boolean] = {
      val left = l.eval[StdVec[Boolean]]
      val right = r.eval[Boolean]
      left.map(f(_, right))
    }
    private[this] def mapb11(l: B1, r: B1, f: (Boolean, Boolean) => Boolean): StdVec[Boolean] = {
      l.eval[StdVec[Boolean]].zip(r.eval[StdVec[Boolean]]).map { case (a, b) => f(a, b) }
    }


    private[this] def eq(l: Double, r: Double): Boolean = l == r
    private[this] def neq(l: Double, r: Double): Boolean = l != r
    private[this] def lt(l: Double, r: Double): Boolean = l < r
    private[this] def lte(l: Double, r: Double): Boolean = l <= r
    private[this] def gt(l: Double, r: Double): Boolean = l > r
    private[this] def gte(l: Double, r: Double): Boolean = l >= r

    private[this] def and(l: Boolean, r: Boolean): Boolean = l & r
    private[this] def or(l: Boolean, r: Boolean): Boolean = l |  r

    def eval(n: B1): StdVec[Boolean] = n match {
      case Eq01 (l: N0, r: N1) => map01(l, r, eq)
      case Eq10 (l: N1, r: N0) => map10(l, r, eq)
      case Eq11 (l: N1, r: N1) => map11(l, r, eq)
      case Neq01(l: N0, r: N1) => map01(l, r, neq)
      case Neq10(l: N1, r: N0) => map10(l, r, neq)
      case Neq11(l: N1, r: N1) => map11(l, r, neq)
      case Lt01 (l: N0, r: N1) => map01(l, r, lt)
      case Lt10 (l: N1, r: N0) => map10(l, r, lt)
      case Lt11 (l: N1, r: N1) => map11(l, r, lt)
      case Lte01(l: N0, r: N1) => map01(l, r, lte)
      case Lte10(l: N1, r: N0) => map10(l, r, lte)
      case Lte11(l: N1, r: N1) => map11(l, r, lte)
      case Gt01 (l: N0, r: N1) => map01(l, r, gt)
      case Gt10 (l: N1, r: N0) => map10(l, r, gt)
      case Gt11 (l: N1, r: N1) => map11(l, r, gt)
      case Gte01(l: N0, r: N1) => map01(l, r, gte)
      case Gte10(l: N1, r: N0) => map10(l, r, gte)
      case Gte11(l: N1, r: N1) => map11(l, r, gte)

      case And01(l: B0, r: B1) => mapb01(l, r, and)
      case And10(l: B1, r: B0) => mapb10(l, r, and)
      case And11(l: B1, r: B1) => mapb11(l, r, and)
      case Or01 (l: B0, r: B1) => mapb01(l, r, or)
      case Or10 (l: B1, r: B0) => mapb10(l, r, or)
      case Or11 (l: B1, r: B1) => mapb11(l, r, or)

      case Not1(v: B1) => v.eval[StdVec[Boolean]].map(!_)
    }
  }

}
