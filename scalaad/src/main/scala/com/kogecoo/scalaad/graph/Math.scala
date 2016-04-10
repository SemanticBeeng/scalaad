package com.kogecoo.scalaad.graph

import com.kogecoo.scalaad.{Shape0, Shape1, Shape2}


// Element-wise Sin
case class Sin0(v: N0) extends Op0
case class Sin1(v: N1) extends Op1
case class Sin2(v: N2) extends Op2

// Element-wise Cos
case class Cos0(v: N0) extends Op0
case class Cos1(v: N1) extends Op1
case class Cos2(v: N2) extends Op2

// Element-wise Tan
case class Tan0(v: N0) extends Op0
case class Tan1(v: N1) extends Op1
case class Tan2(v: N2) extends Op2

// Element-wise Asin
case class Asin0(v: N0) extends Op0
case class Asin1(v: N1) extends Op1
case class Asin2(v: N2) extends Op2

// Element-wise Acos
case class Acos0(v: N0) extends Op0
case class Acos1(v: N1) extends Op1
case class Acos2(v: N2) extends Op2

// Element-wise Atan
case class Atan0(v: N0) extends Op0
case class Atan1(v: N1) extends Op1
case class Atan2(v: N2) extends Op2

// Element-wise Sinh
case class Sinh0(v: N0) extends Op0
case class Sinh1(v: N1) extends Op1
case class Sinh2(v: N2) extends Op2

// Element-wise Cosh
case class Cosh0(v: N0) extends Op0
case class Cosh1(v: N1) extends Op1
case class Cosh2(v: N2) extends Op2

// Element-wise Tanh
case class Tanh0(v: N0) extends Op0
case class Tanh1(v: N1) extends Op1
case class Tanh2(v: N2) extends Op2

// Element-wise Ln
case class Ln0(v: N0) extends Op0
case class Ln1(v: N1) extends Op1
case class Ln2(v: N2) extends Op2

// Element-wise Exp
case class Exp0(v: N0) extends Op0
case class Exp1(v: N1) extends Op1
case class Exp2(v: N2) extends Op2

// Element-wise Sqrt
case class Sqrt0(v: N0) extends Op0
case class Sqrt1(v: N1) extends Op1
case class Sqrt2(v: N2) extends Op2

// Element-wise Pow
case class Pow00(l: N0, r: N0) extends Op00
case class Pow11(l: N1, r: N1) extends Op11
case class Pow22(l: N2, r: N2) extends Op22

// Broadcast Pow
case class Pow01(l: N0, r: N1) extends Op01
case class Pow10(l: N1, r: N0) extends Op10
case class Pow02(l: N0, r: N2) extends Op02
case class Pow20(l: N2, r: N0) extends Op20
case class Pow21(l: N2, r: N1) extends Op21
case class Pow12(l: N1, r: N2) extends Op12

// Element-wise Abs
case class Abs0(v: N0) extends Op0
case class Abs1(v: N1) extends Op1
case class Abs2(v: N2) extends Op2

// Element-wise Max
case class Max00(l: N0, r: N0) extends Op00
case class Max11(l: N1, r: N1) extends Op11
case class Max22(l: N2, r: N2) extends Op22

// Broadcast Max
case class Max01(l: N0, r: N1) extends Op01
case class Max02(l: N0, r: N2) extends Op02
case class Max10(l: N1, r: N0) extends Op10
case class Max20(l: N2, r: N0) extends Op20
case class Max12(l: N1, r: N2) extends Op12
case class Max21(l: N2, r: N1) extends Op21

// Element-wise Min
case class Min00(l: N0, r: N0) extends Op00
case class Min11(l: N1, r: N1) extends Op11
case class Min22(l: N2, r: N2) extends Op22

// Broadcast Min
case class Min01(l: N0, r: N1) extends Op01
case class Min02(l: N0, r: N2) extends Op02
case class Min10(l: N1, r: N0) extends Op10
case class Min20(l: N2, r: N0) extends Op20
case class Min12(l: N1, r: N2) extends Op12
case class Min21(l: N2, r: N1) extends Op21

// Experimental

// Norm
case class L0Norm(v: N1) extends UnaryOp[S0, S1] { val shape: S0 = Shape0() }
case class L1Norm(v: N1) extends UnaryOp[S0, S1] { val shape: S0 = Shape0() }
case class L2Norm(v: N1) extends UnaryOp[S0, S1] { val shape: S0 = Shape0() }

// Dot
case class Dot11(l: N1, r: N1) extends BinaryOp[S0, S1, S1] { val shape: S0 = Shape0() }

// order 0 value is assumed to be expand to order 1 with same shape.
case class Dot01(l: N0, r: N1) extends BinaryOp[S0, S0, S1] { val shape: S0 = Shape0() }
case class Dot10(l: N1, r: N0) extends BinaryOp[S0, S1, S0] { val shape: S0 = Shape0() }

// Matmul
// assume that l is a row vector
case class MatMulR12(l: N1, r: N2) extends BinaryOp[S1, S1, S2] {
  val shape: S1 = Shape1(r.shape._2, transposed = true)
}

// assume that l is a column vector (so r shaped (1, k) is only allowed)
case class MatMulC12(l: N1, r: N2) extends BinaryOp[S2, S1, S2] {
  val shape: S2 = Shape2(l.shape._1, r.shape._2)
}

// assume that r is a column vector
case class MatMul2C1(l: N2, r: N1) extends BinaryOp[S1, S2, S1] {
  val shape: S1 = Shape1(l.shape._1, transposed = false)
}

// assume that r is a row vector (so l shaped (k, 1) is only allowed)
case class MatMul2R1(l: N2, r: N1) extends BinaryOp[S2, S2, S1] {
  val shape: S2 = Shape2(l.shape._1, r.shape._1)
}

case class MatMul22(l: N2, r: N2) extends BinaryOp[S2, S2, S2] {
  val shape: S2 = Shape2(l.shape._1, r.shape._2)
}

