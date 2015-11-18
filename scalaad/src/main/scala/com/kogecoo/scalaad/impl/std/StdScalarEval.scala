package com.kogecoo.scalaad.impl.std

import com.kogecoo.scalaad.algorithm.Eval
import com.kogecoo.scalaad.graph._
import com.kogecoo.scalaad.impl.std.Implicits._


trait StdScalarEval {

  implicit val eval_bool00_double: Eval[B0, Boolean] = new Eval[B0, Boolean] {

    def eval(n: B0): Boolean = n match {
      case Eq00 (l: N0, r: N0) => l.eval[Double] == r.eval[Double]
      case Neq00(l: N0, r: N0) => l.eval[Double] != r.eval[Double]
      case Lt00 (l: N0, r: N0) => l.eval[Double] <  r.eval[Double]
      case Lte00(l: N0, r: N0) => l.eval[Double] <= r.eval[Double]
      case Gt00 (l: N0, r: N0) => l.eval[Double] >  r.eval[Double]
      case Gte00(l: N0, r: N0) => l.eval[Double] >= r.eval[Double]

      case And00(l: B0, r: B0) => l.eval[Boolean] && r.eval[Boolean]
      case Or00(l: B0, r: B0)  => l.eval[Boolean] || r.eval[Boolean]

      case Not0(v: B0) => !v.eval[Boolean]
    }
  }

  // operators/functions convert Node order 0 -> 0
  implicit val eval00_double: Eval[N0, Double] = new Eval[N0, Double] {
    private[this] type N = N0
    private[this] type V = Double

    def eval(n: N): V = n match {

      // Leaf nodes
      case Var0(v)          => v.value[V]
      case ArbVar0(_, data) => data.get.value[V]
      case Zero0()          => 0.0
      case Half0()          => 0.5
      case One0()           => 1.0
      case Const0(v)        => v.value[V]

      // Unary ops
      case Pos0(v: N) => +v.eval[V]
      case Neg0(v: N) => -v.eval[V]

      // Binary ops
      case Add00(l: N, r: N) => l.eval[V] + r.eval[V]
      case Sub00(l: N, r: N) => l.eval[V] - r.eval[V]
      case Mul00(l: N, r: N) => l.eval[V] * r.eval[V]
      case Div00(l: N, r: N) => l.eval[V] / r.eval[V]

      // Math
      case Sin0(v: N) => math.sin(v.eval[V])
      case Cos0(v: N) => math.cos(v.eval[V])
      case Tan0(v: N) => math.tan(v.eval[V])

      case Asin0(v: N) => math.asin(v.eval[V])
      case Acos0(v: N) => math.acos(v.eval[V])
      case Atan0(v: N) => math.atan(v.eval[V])

      case Sinh0(v: N) => math.sinh(v.eval[V])
      case Cosh0(v: N) => math.cosh(v.eval[V])
      case Tanh0(v: N) => math.tanh(v.eval[V])

      case Ln0(v: N)         => math.log(v.eval[V])
      case Exp0(v: N)        => math.exp(v.eval[V])
      case Sqrt0(v: N)       => math.sqrt(v.eval[V])
      case Pow00(l: N, r: N) => math.pow(l.eval[V], r.eval[V])

      case Abs0(v: N)        => math.abs(v.eval[V])
      case Max00(l: N, r: N) => math.max(l.eval[V], r.eval[V])
      case Min00(l: N, r: N) => math.max(l.eval[V], r.eval[V])

      case Where0_0(cond: B0, a: N, b: N) => if (cond.eval[Boolean]) a.eval[V] else b.eval[V]
    }
  }

}
