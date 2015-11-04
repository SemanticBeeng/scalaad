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
}
