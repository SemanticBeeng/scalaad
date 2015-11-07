package com.kogecoo.scalaad

import shapeless.{Sized, Nat}
import shapeless.ops.nat.Diff
import shapeless.ops.nat.LT.<


package object graph {

  //  Aliases for containers used for carrying
  // {0, 1, 2, k}-order tensor shape (dimensions for each axis) parameter(s).
  type S0 = Unit
  type S1 = Int
  type S2 = (Int, Int)
  type S[K <: Nat] = Sized[Seq, K]

  // Aliases for rank specified Node
  type N0[T] = Node[T, S0]
  type N1[T] = Node[T, S1]
  type N2[T] = Node[T, S2]
  type NK[T, K] = Node[T, S[K]]

  type Op00[T] = BinaryOp[T, S0, N0[T], N0[T]]
  type Op01[T] = BinaryOp[T, S1, N0[T], N1[T]]
  type Op10[T] = BinaryOp[T, S1, N1[T], N0[T]]
  type Op11[T] = BinaryOp[T, S1, N1[T], N1[T]]
  type Op02[T] = BinaryOp[T, S2, N0[T], N2[T]]
  type Op20[T] = BinaryOp[T, S2, N2[T], N0[T]]
  type Op22[T] = BinaryOp[T, S2, N2[T], N2[T]]

  implicit def n0[T: Numeric](x: T): N0[T] = Const(x)
  implicit class N0Cons[T](l: N0[T]) extends AnyVal {
    def +(r: N0[T]): N0[T] = Add00(l, r)
  }

  val x: N0[Float] = 0.0f + 0.0f

}
