package com.kogecoo.scalaad

import shapeless.Nat
import shapeless.ops.nat.LT.<


package object graph {

  type Eq0[K] = K =:= Nat._0
  type Eq1[K] = K =:= Nat._1
  type Eq2[K] = K =:= Nat._2
  type Gt0[K] = Nat._0 < K
  type Gt1[K] = Nat._1 < K
  type Gt2[K] = Nat._2 < K
  type Lte1[K] = K < Nat._2
  type Lte2[K] = K < Nat._3

  type N0[T] = Node0[T]
  type N1[U, T] = Node1[U, T]
  type N2[U, T] = Node2[U, T]
  type NK[Rank] = Node[Rank]
}
