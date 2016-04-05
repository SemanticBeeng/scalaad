package com.kogecoo.scalaad.impl.std

import com.kogecoo.scalaad.StdVec
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
    private[this] type T0 = Double
    private[this] type T1 = StdVec[T0]

    def eval(n: N): T0 = n match {

      // Leaf nodes
      case Var0(v)          => v.value[T0]
      case ArbVar0(_, data) => data.get.value[T0]
      case Zero0()          => 0.0
      case Half0()          => 0.5
      case One0()           => 1.0
      case Const0(v)        => v.value[T0]

      // Unary ops
      case Pos0(v: N) => +v.eval[T0]
      case Neg0(v: N) => -v.eval[T0]

      // Binary ops
      case Add00(l: N, r: N) => l.eval[T0] + r.eval[T0]
      case Sub00(l: N, r: N) => l.eval[T0] - r.eval[T0]
      case Mul00(l: N, r: N) => l.eval[T0] * r.eval[T0]
      case Div00(l: N, r: N) => l.eval[T0] / r.eval[T0]

      // Math
      case Sin0(v: N) => math.sin(v.eval[T0])
      case Cos0(v: N) => math.cos(v.eval[T0])
      case Tan0(v: N) => math.tan(v.eval[T0])

      case Asin0(v: N) => math.asin(v.eval[T0])
      case Acos0(v: N) => math.acos(v.eval[T0])
      case Atan0(v: N) => math.atan(v.eval[T0])

      case Sinh0(v: N) => math.sinh(v.eval[T0])
      case Cosh0(v: N) => math.cosh(v.eval[T0])
      case Tanh0(v: N) => math.tanh(v.eval[T0])

      case Ln0(v: N)         => math.log(v.eval[T0])
      case Exp0(v: N)        => math.exp(v.eval[T0])
      case Sqrt0(v: N)       => math.sqrt(v.eval[T0])
      case Pow00(l: N, r: N) => math.pow(l.eval[T0], r.eval[T0])

      case Dot01(l: N0, r: N1) => r.eval[T1].map({ y => l.eval[T0] * y }).sum
      case Dot10(l: N1, r: N0) => l.eval[T1].map({ x => x * r.eval[T0] }).sum
      case Dot11(l: N1, r: N1) => l.eval[T1].zip(r.eval[T1]).map({ case (x, y) => x * y }).sum

      case Abs0(v: N)        => math.abs(v.eval[T0])
      case Max00(l: N, r: N) => math.max(l.eval[T0], r.eval[T0])
      case Min00(l: N, r: N) => math.min(l.eval[T0], r.eval[T0])

      case Where0_0(cond: B0, a: N, b: N) => if (cond.eval[Boolean]) a.eval[T0] else b.eval[T0]
    }
  }

}
