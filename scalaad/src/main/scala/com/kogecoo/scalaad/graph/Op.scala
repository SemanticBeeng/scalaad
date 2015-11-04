package com.kogecoo.scalaad.graph


// Element-wise Add
case class Add00(l :N0, r: N0) extends Op00
case class Add11(l :N1, r: N1) extends Op11
case class Add22(l :N2, r: N2) extends Op22

// Broadcast Add
case class Add01(l :N0, r: N1) extends Op01
case class Add10(l :N1, r: N0) extends Op10
case class Add02(l :N0, r: N2) extends Op02
case class Add20(l :N2, r: N0) extends Op20
case class Add12(l :N1, r: N2) extends Op12
case class Add21(l :N2, r: N1) extends Op21

// Element-wise Sub
case class Sub00(l :N0, r: N0) extends Op00
case class Sub11(l :N1, r: N1) extends Op11
case class Sub22(l :N2, r: N2) extends Op22

// Broadcast Sub
case class Sub01(l :N0, r: N1) extends Op01
case class Sub10(l :N1, r: N0) extends Op10
case class Sub02(l :N0, r: N2) extends Op02
case class Sub20(l :N2, r: N0) extends Op20
case class Sub12(l :N1, r: N2) extends Op12
case class Sub21(l :N2, r: N1) extends Op21

// Element-wise Mul
case class Mul00(l :N0, r: N0) extends Op00
case class Mul11(l :N1, r: N1) extends Op11
case class Mul22(l :N2, r: N2) extends Op22

// Broadcast Mul
case class Mul01(l :N0, r: N1) extends Op01
case class Mul10(l :N1, r: N0) extends Op10
case class Mul02(l :N0, r: N2) extends Op02
case class Mul20(l :N2, r: N0) extends Op20
case class Mul12(l :N1, r: N2) extends Op12
case class Mul21(l :N2, r: N1) extends Op21

// Element-wise Div
case class Div00(l :N0, r: N0) extends Op00
case class Div11(l :N1, r: N1) extends Op11
case class Div22(l :N2, r: N2) extends Op22

// Broadcast Div
case class Div01(l :N0, r: N1) extends Op01
case class Div10(l :N1, r: N0) extends Op10
case class Div02(l :N0, r: N2) extends Op02
case class Div20(l :N2, r: N0) extends Op20
case class Div12(l :N1, r: N2) extends Op12
case class Div21(l :N2, r: N1) extends Op21

// Element-wise Pos
case class Pos0(v: N0) extends Op0
case class Pos1(v: N1) extends Op1
case class Pos2(v: N2) extends Op2

// Element-wise Neg
case class Neg0(v: N0) extends Op0
case class Neg1(v: N1) extends Op1
case class Neg2(v: N2) extends Op2

// Transpose
case class Transpose1(v: N1) extends Op1
case class Transpose2(v: N2) extends Op2


