package com.kogecoo.scalaad.impl.std

import com.kogecoo.scalaad.{StdVec, StdMat}
import com.kogecoo.scalaad.algorithm.Eval
import com.kogecoo.scalaad.graph._
import com.kogecoo.scalaad.impl.std.Implicits._


trait StdMatEval {

  private[this] type T = Double
  private[this] type V = StdVec[T]
  private[this] type M = StdMat[T]
  private[this] type UOp = T => T
  private[this] type BOp = (T, T) => T

  private[this] def eye(shape: S2): StdMat[Double] = {
    (0 until shape._1).map { i =>
      (0 until shape._2).map { j =>
        if (i == j) 1.0 else 0.0
      }
    }
  }

  private[this] def map1(v: N1, f: UOp): V = v.eval[V].map(f(_))
  private[this] def map2(v: N2, f: UOp): M = v.eval[M].map(_.map(f(_)))

  private[this] def map02(l: N0, r: N2, f: BOp): M = r.eval[M].map(_.map(f(l.eval[T], _)))
  private[this] def map20(l: N2, r: N0, f: BOp): M = l.eval[M].map(_.map(f(_, r.eval[T])))
  private[this] def map22(l: N2, r: N2, f: BOp): M = {
    l.eval[M].zip(r.eval[M]).map { case (x, y) =>
      x.zip(y).map { case (a, b) => f(a, b) }
    }
  }

  private[this] def map12row(l: N1, r: N2, f: BOp): M = r.eval[M].map(l.eval[V].zip(_).map { case (x, y) => f(x, y) })
  private[this] def map21row(l: N2, r: N1, f: BOp): M = l.eval[M].map(_.zip(r.eval[V]).map { case (x, y) => f(x, y) })
  private[this] def map12col(l: N1, r: N2, f: BOp): M = r.eval[M].zip(l.eval[V]).map { case (y, x) => y.map(f(_, x)) }
  private[this] def map21col(l: N2, r: N1, f: BOp): M = l.eval[M].zip(r.eval[V]).map { case (x, y) => x.map(f(_, y)) }

  private[this] def map12(l: N1, r: N2, f: BOp): M = if (l.shape.transposed) map12row(l, r, f) else map12col(l, r, f)
  private[this] def map21(l: N2, r: N1, f: BOp): M = if (r.shape.transposed) map21row(l, r, f) else map21col(l, r, f)

  private[this] def pos(v: T): T = +v
  private[this] def neg(v: T): T = -v
  private[this] def add(l: T, r: T): T = l + r
  private[this] def sub(l: T, r: T): T = l - r
  private[this] def mul(l: T, r: T): T = l * r
  private[this] def div(l: T, r: T): T = l / r

  implicit val eval22_stdmat_double: Eval[N2, M] = new Eval[N2, M] {

    def eval(n: N2): M = n match {

      // Leaf nodes
      case Var2(v, _)                => v.value[M]
      case ArbVar2(sig, data, shape) => data.get.value[M]
      case Zero2(shape)              => Seq.fill(shape._1, shape._2)(0.0)
      case Half2(shape)              => Seq.fill(shape._1, shape._2)(0.5)
      case One2(shape)               => Seq.fill(shape._1, shape._2)(1.0)
      case Const2(v, shape)          => v.value[M]
      case Eye2(shape)               => eye(shape)

      // Unary ops
      case Pos2(v) => map2(v, pos)
      case Neg2(v) => map2(v, neg)
      case Transpose2(Transpose2(v)) => v.eval[M]
      case Transpose2(v) => { // maybe too slow
        val m = v.eval[M]
        val rows = m.shape._1
        val cols = m.shape._2
        (0 until cols).map { c =>
          (0 until rows).map { r =>
            m(r)(c)
          }
        }
      }

      // Binary ops
      case Add02(l: N0, r: N2) => map02(l, r, add)
      case Add20(l: N2, r: N0) => map20(l, r, add)
      case Add22(l: N2, r: N2) => map22(l, r, add)

      case Sub02(l: N0, r: N2) => map02(l, r, sub)
      case Sub20(l: N2, r: N0) => map20(l, r, sub)
      case Sub22(l: N2, r: N2) => map22(l, r, sub)

      case Mul02(l: N0, r: N2) => map02(l, r, mul)
      case Mul12(l: N1, r: N2) => map12(l, r, mul)
      case Mul20(l: N2, r: N0) => map20(l, r, mul)
      case Mul21(l: N2, r: N1) => map21(l, r, mul)
      case Mul22(l: N2, r: N2) => map22(l, r, mul)

      case Div02(l: N0, r: N2) => map02(l, r, div)
      case Div12(l: N1, r: N2) => map12(l, r, div)
      case Div20(l: N2, r: N0) => map20(l, r, div)
      case Div21(l: N2, r: N1) => map21(l, r, div)
      case Div22(l: N2, r: N2) => map22(l, r, div)

      // Math
      case Sin2(v) => map2(v, math.sin)
      case Cos2(v) => map2(v, math.cos)
      case Tan2(v) => map2(v, math.tan)

      case Asin2(v) => map2(v, math.asin)
      case Acos2(v) => map2(v, math.acos)
      case Atan2(v) => map2(v, math.atan)

      case Sinh2(v) => map2(v, math.sinh)
      case Cosh2(v) => map2(v, math.cosh)
      case Tanh2(v) => map2(v, math.tanh)

      case Ln2(v)              => map2(v, math.log)
      case Exp2(v: N2)         => map2(v, math.exp)
      case Sqrt2(v: N2)        => map2(v, math.sqrt)
      case Pow02(l: N0, r: N2) => map02(l, r, math.pow)
      case Pow20(l: N2, r: N0) => map20(l, r, math.pow)
      case Pow22(l: N2, r: N2) => map22(l, r, math.pow)

      // Experimental
      case Abs2(v)             => map2(v, math.abs)
      case Max02(l: N0, r: N2) => map02(l, r, math.max)
      case Max20(l: N2, r: N0) => map20(l, r, math.max)
      case Max22(l: N2, r: N2) => map22(l, r, math.max)
      case Min02(l: N0, r: N2) => map02(l, r, math.min)
      case Min20(l: N2, r: N0) => map20(l, r, math.min)
      case Min22(l: N2, r: N2) => map22(l, r, math.min)

      case Where0_2(cond: B0, a: N2, b: N2) => {
        if (cond.eval[Boolean]) a.eval[StdMat[T]] else b.eval[StdMat[T]]
      }
      case Where1_2(cond: B1, a: N2, b: N2) => {
        val ae = a.eval[StdMat[T]]
        val be = b.eval[StdMat[T]]
        val ce = cond.eval[StdVec[Boolean]]
        ce.zip(ae.zip(be)).map { case (c, (x, y))  => if (c) x else y }
      }

      case Where2_2(cond: B2, a: N2, b: N2) => {
        val x = a.eval[M]
        val y = b.eval[M]
        cond.eval[StdMat[Boolean]].zipWithIndex.map { case (r, i) =>
          r.zipWithIndex.map { case (c, j) => if (c) x(i)(j) else y(i)(j) }
        }
      }

      case MatMul22(a: N2, b: N2) => {  //assert(a(0).size == b.size)
        val aeval = a.eval[StdMat[T]]
        val beval = b.eval[StdMat[T]]
        assert(aeval.head.size == beval.size)
        (0 until b.shape._2).map { bcolIndex =>
          aeval.map { arow =>
            arow.zip(beval.map(_.apply(bcolIndex))).map({ case (x, y) => x * y }).sum
          }
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
