package com.kogecoo.scalaad.graph


case class Add00[T](left :N0[T], right: N0[T]) extends Op00[T]
case class Add01[T](left :N0[T], right: N1[T]) extends Op01[T]
case class Add10[T](left :N1[T], right: N0[T]) extends Op10[T]
case class Add11[T](left :N1[T], right: N1[T]) extends Op11[T]
case class Add02[T](left :N0[T], right: N0[T]) extends Op02[T]
case class Add20[T](left :N0[T], right: N0[T]) extends Op20[T]
case class Add22[T](left :N0[T], right: N0[T]) extends Op22[T]

case class Sub00[T](left :N0[T], right: N0[T]) extends Op00[T]
case class Sub01[T](left :N0[T], right: N1[T]) extends Op01[T]
case class Sub10[T](left :N1[T], right: N0[T]) extends Op10[T]
case class Sub11[T](left :N1[T], right: N1[T]) extends Op11[T]
case class Sub02[T](left :N0[T], right: N0[T]) extends Op02[T]
case class Sub20[T](left :N0[T], right: N0[T]) extends Op20[T]
case class Sub22[T](left :N0[T], right: N0[T]) extends Op22[T]

case class Mul00[T](left :N0[T], right: N0[T]) extends Op00[T]
case class Mul01[T](left :N0[T], right: N1[T]) extends Op01[T]
case class Mul10[T](left :N1[T], right: N0[T]) extends Op10[T]
case class Mul11[T](left :N1[T], right: N1[T]) extends Op11[T]
case class Mul02[T](left :N0[T], right: N0[T]) extends Op02[T]
case class Mul20[T](left :N0[T], right: N0[T]) extends Op20[T]
case class Mul22[T](left :N0[T], right: N0[T]) extends Op22[T]

case class Div00[T](left :N0[T], right: N0[T]) extends Op00[T]
case class Div01[T](left :N0[T], right: N1[T]) extends Op01[T]
case class Div10[T](left :N1[T], right: N0[T]) extends Op10[T]
case class Div11[T](left :N1[T], right: N1[T]) extends Op11[T]
case class Div02[T](left :N0[T], right: N0[T]) extends Op02[T]
case class Div20[T](left :N0[T], right: N0[T]) extends Op20[T]
case class Div22[T](left :N0[T], right: N0[T]) extends Op22[T]

