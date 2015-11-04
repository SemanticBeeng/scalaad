package com.kogecoo.scalaad.eval

import com.kogecoo.scalaad.graph.{Add, Node}
import shapeless.Nat

import scala.language.higherKinds

trait Eval[U[_], T] {
/*
  def eval[RankL, RankR, EvRank, N <: Node[U, T, EvRank]](n: N)(implicit ev: N =:= Add, e: RankL =:= Nat._0) = {

  }
  def eval[RankL, RankR, EvRank, N <: Node[U, T, EvRank]](n: N)(implicit ev: N =:= Add[U, T, RankL, RankR, EvRank], e: RankL =:= Nat._0) = {

  }
  */
}
