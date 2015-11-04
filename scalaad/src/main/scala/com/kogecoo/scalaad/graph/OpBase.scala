package com.kogecoo.scalaad.graph

import shapeless.Nat
import shapeless.ops.nat.Max

import scala.language.higherKinds


trait UnaryOp[U[_], T, Rank <: Nat, OutRank <: Nat] extends Node[U, T, OutRank]

trait BinaryOp[U[_], T, RankL <: Nat, RankR <: Nat, OutRank <: Nat] extends Node[U, T, OutRank]
