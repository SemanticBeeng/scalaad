package com.kogecoo.scalaad.impl.std

import com.kogecoo.scalaad.{StdVec, StdMat}
import com.kogecoo.scalaad.algorithm.Eval
import com.kogecoo.scalaad.graph._
import com.kogecoo.scalaad.impl.std.Implicits._

import scala.reflect.ClassTag


trait StdMatEval {

  private[this] type V1[T] = StdVec[T]
  private[this] type V2[T] = StdMat[T]

  private[this] def fillMat[T: ClassTag](value: T, shape: S2): V2[T] = {
    Seq.fill[V1[T]](shape._1)(
      Seq.fill[T](shape._2)(value)
    )
  }

  private[this] def map1[T](v: V1[T], f: T => T): V1[T] = v.map(f(_))
  private[this] def map2[T](v: V2[T], f: T => T): V2[T] = v.map(_.map(f(_)))

  private[this] def map02[T](l: T    , r: V2[T], f: (T, T) => T): V2[T] = r.map(_.map(f(l, _)))
  private[this] def map20[T](l: V2[T], r: T    , f: (T, T) => T): V2[T] = l.map(_.map(f(_, r)))
  private[this] def map12[T](l: V1[T], r: V2[T], f: (T, T) => T): V2[T] = r.map(l.zip(_).map { case (x, y) => f(x, y) })
  private[this] def map21[T](l: V2[T], r: V1[T], f: (T, T) => T): V2[T] = l.map(_.zip(r).map { case (x, y) => f(x, y) })
  private[this] def map22[T](l: V2[T], r: V2[T], f: (T, T) => T): V2[T] = l.zip(r).map { case (x, y) => x.zip(y).map { case (a, b) => f(a, b) } }

  private[this] def pos(v: Double): Double = +v
  private[this] def neg(v: Double): Double = -v
  private[this] def add(l: Double, r: Double): Double = l + r
  private[this] def sub(l: Double, r: Double): Double = l - r
  private[this] def mul(l: Double, r: Double): Double = l * r
  private[this] def div(l: Double, r: Double): Double = l / r

  implicit val eval22_stdmat_double: Eval[N2, StdMat[Double]] = new Eval[N2, StdMat[Double]] {
    private[this] type T = Double
    private[this] type V = StdMat[T]
    private[this] type N = N2

    def eval(n: N): V = n match {

      // Leaf nodes
      case Var2(v, _)                => v.value[V]
      case ArbVar2(sig, data, shape) => data.get.value[V]
      case Zero2(shape)              => fillMat(0.0, shape)
      case Half2(shape)              => fillMat(0.5, shape)
      case One2(shape)               => fillMat(1.0, shape)
      case Const2(v, shape)          => v.value[V]

      // Unary ops
      case Pos2(v) => map2(v.eval[V], pos)
      case Neg2(v) => map2(v.eval[V], neg)

      // Binary ops
      case Add02(l: N0, r: N)  => map02[T](l.eval[T], r.eval[V], add)
      case Add20(l: N , r: N0) => map20[T](l.eval[V], r.eval[T], add)
      case Add22(l: N , r: N)  => map22[T](l.eval[V], r.eval[V], add)

      case Sub02(l: N0, r: N)  => map02[T](l.eval[T], r.eval[V], sub)
      case Sub20(l: N , r: N0) => map20[T](l.eval[V], r.eval[T], sub)
      case Sub22(l: N , r: N)  => map22[T](l.eval[V], r.eval[V], sub)

      case Mul02(l: N0, r: N)  => map02[T](l.eval[T], r.eval[V], mul)
      case Mul20(l: N , r: N0) => map20[T](l.eval[V], r.eval[T], mul)
      case Mul22(l: N , r: N)  => map22[T](l.eval[V], r.eval[V], mul)

      case Div02(l: N0, r: N)  => map02[T](l.eval[T], r.eval[V], div)
      case Div20(l: N , r: N0) => map20[T](l.eval[V], r.eval[T], div)
      case Div22(l: N , r: N)  => map22[T](l.eval[V], r.eval[V], div)

      // Math
      case Sin2(v) => map2(v.eval[V], math.sin)
      case Cos2(v) => map2(v.eval[V], math.cos)
      case Tan2(v) => map2(v.eval[V], math.tan)

      case Asin2(v) => map2(v.eval[V], math.asin)
      case Acos2(v) => map2(v.eval[V], math.acos)
      case Atan2(v) => map2(v.eval[V], math.atan)

      case Sinh2(v) => map2(v.eval[V], math.sinh)
      case Cosh2(v) => map2(v.eval[V], math.cosh)
      case Tanh2(v) => map2(v.eval[V], math.tanh)

      case Ln2(v)              => map2(v.eval[V], math.log)
      case Exp2(v: N)          => map2(v.eval[V], math.exp)
      case Sqrt2(v: N)         => map2(v.eval[V], math.sqrt)
      case Pow02(l: N0, r: N)  => map02[T](l.eval[T], r.eval[V], math.pow)
      case Pow20(l: N , r: N0) => map20[T](l.eval[V], r.eval[T], math.pow)
      case Pow22(l: N , r: N)  => map22[T](l.eval[V], r.eval[V], math.pow)

      // Experimental
      case Abs2(v)                => map2[T](v.eval[V], math.abs)
      case Max02(l: N0   , r: N)  => map02[T](l.eval[T], r.eval[V], math.max)
      case Max20(l: N    , r: N0) => map20[T](l.eval[V], r.eval[T], math.max)
      case Max22(l: N    , r: N)  => map22[T](l.eval[V], r.eval[V], math.max)
      case Min02(l: N0   , r: N)  => map02[T](l.eval[T], r.eval[V], math.min)
      case Min20(l: N    , r: N0) => map20[T](l.eval[V], r.eval[T], math.min)
      case Min22(l: N    , r: N)  => map22[T](l.eval[V], r.eval[V], math.min)

      case Where0_2(cond: B0, a: N, b: N) => {
        if (cond.eval[Boolean]) a.eval[StdMat[T]] else b.eval[StdMat[T]]
      }
      case Where1_2(cond: B1, a: N, b: N) => {
        val ae = a.eval[StdMat[T]]
        val be = b.eval[StdMat[T]]
        val ce = cond.eval[StdVec[Boolean]]
        ce.zip(ae.zip(be)).map { case (c, (x, y))  => if (c) x else y }
      }

      case Where2_2(cond: B2, a: N, b: N) => {
        val x = a.eval[V]
        val y = b.eval[V]
        cond.eval[StdMat[Boolean]].zipWithIndex.map { case (r, i) =>
          r.zipWithIndex.map { case (c, j) => if (c) x(i)(j) else y(i)(j) }
        }
      }
    }
  }

  // Experimental
  /*implicit val eval20_stdmat_dobule: Eval[N2, Double] = new Eval[N2, Double] {

    private[this] type T = Double
    private[this] type V = StdMat[T]

    def eval(n: N2): T = n match {
      case Max2(v: N2) => v.eval[V].map(_.max).max
      case Min2(v: N2) => v.eval[V].map(_.min).min
    }
  }*/

  implicit val eval_bool22_stdmat_double: Eval[B2, StdMat[Boolean]] = new Eval[B2, StdMat[Boolean]] {

    private[this] def map02(l: N0, r: N2, f: (Double, Double) => Boolean): StdMat[Boolean] = {
      val left = l.eval[Double]
      val right = r.eval[StdMat[Double]]
      right.map(_.map(f(left, _)))
    }

    private[this] def map20(l: N2, r: N0, f: (Double, Double) => Boolean): StdMat[Boolean] = {
      val left = l.eval[StdMat[Double]]
      val right = r.eval[Double]
      left.map(_.map(f(_, right)))
    }
    private[this] def map22(l: N2, r: N2, f: (Double, Double) => Boolean): StdMat[Boolean] = {
      l.eval[StdMat[Double]].zip(r.eval[StdMat[Double]]).map { case (a, b) =>
        a.zip(b).map { case (x, y) => f(x, y) }
      }
    }

    private[this] def mapb02(l: B0, r: B2, f: (Boolean, Boolean) => Boolean): StdMat[Boolean] = {
      val left = l.eval[Boolean]
      val right = r.eval[StdMat[Boolean]]
      right.map(_.map(f(left, _)))
    }

    private[this] def mapb20(l: B2, r: B0, f: (Boolean, Boolean) => Boolean): StdMat[Boolean] = {
      val left = l.eval[StdMat[Boolean]]
      val right = r.eval[Boolean]
      left.map(_.map(f(_, right)))
    }
    private[this] def mapb22(l: B2, r: B2, f: (Boolean, Boolean) => Boolean): StdMat[Boolean] = {
      l.eval[StdMat[Boolean]].zip(r.eval[StdMat[Boolean]]).map { case (a, b) =>
        a.zip(b).map { case (x, y) => f(x, y) }
      }
    }


    private[this] def eq(l: Double, r: Double): Boolean = l == r
    private[this] def neq(l: Double, r: Double): Boolean = l != r
    private[this] def lt(l: Double, r: Double): Boolean = l < r
    private[this] def lte(l: Double, r: Double): Boolean = l <= r
    private[this] def gt(l: Double, r: Double): Boolean = l > r
    private[this] def gte(l: Double, r: Double): Boolean = l >= r

    private[this] def and(l: Boolean, r: Boolean): Boolean = l & r
    private[this] def or(l: Boolean, r: Boolean): Boolean = l |  r

    def eval(n: B2): StdMat[Boolean] = n match {
      case Eq02(l: N0, r: N2)  => map02(l, r, eq)
      case Eq20(l: N2, r: N0)  => map20(l, r, eq)
      case Eq22(l: N2, r: N2)  => map22(l, r, eq)
      case Neq02(l: N0, r: N2) => map02(l, r, neq)
      case Neq20(l: N2, r: N0) => map20(l, r, neq)
      case Neq22(l: N2, r: N2) => map22(l, r, neq)
      case Lt02(l: N0, r: N2)  => map02(l, r, lt)
      case Lt20(l: N2, r: N0)  => map20(l, r, lt)
      case Lt22(l: N2, r: N2)  => map22(l, r, lt)
      case Lte02(l: N0, r: N2) => map02(l, r, lte)
      case Lte20(l: N2, r: N0) => map20(l, r, lte)
      case Lte22(l: N2, r: N2) => map22(l, r, lte)
      case Gt02(l: N0, r: N2)  => map02(l, r, gt)
      case Gt20(l: N2, r: N0)  => map20(l, r, gt)
      case Gt22(l: N2, r: N2)  => map22(l, r, gt)
      case Gte02(l: N0, r: N2) => map02(l, r, gte)
      case Gte20(l: N2, r: N0) => map20(l, r, gte)
      case Gte22(l: N2, r: N2) => map22(l, r, gte)

      case And02(l: B0, r: B2) => mapb02(l, r, and)
      case And20(l: B2, r: B0) => mapb20(l, r, and)
      case And22(l: B2, r: B2) => mapb22(l, r, and)
      case Or02(l: B0, r: B2)  => mapb02(l, r, or)
      case Or20(l: B2, r: B0)  => mapb20(l, r, or)
      case Or22(l: B2, r: B2)  => mapb22(l, r, or)

      case Not2(v: B2) => v.eval[StdMat[Boolean]].map(_.map(!_))
    }
  }

}
