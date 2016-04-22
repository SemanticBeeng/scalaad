package com.kogecoo.scalaad.node

import com.kogecoo.scalaad.Shape


/**
  * (Experimental) if (cond) a else b
  *
  * @tparam S the order of Node cond, a and b
  */
trait WhereBase[S <: Shape, Cond <: Shape] extends ValueTerm[S] {
  override val shape: S = a.shape
  def a: ValueTerm[S]
  def b: ValueTerm[S]
}

// e.g. Where1_1([true, false, false], [1, 2, 3], [-4, -5, -6]) results [1, -5, -6]
case class Where0_0(cond: B0, a: N0, b: N0) extends WhereBase[S0, S0]
case class Where1_1(cond: B1, a: N1, b: N1) extends WhereBase[S1, S1]
case class Where2_2(cond: B2, a: N2, b: N2) extends WhereBase[S2, S2]

case class Where0_1(cond: B0, a: N1, b: N1) extends WhereBase[S1, S0]
case class Where0_2(cond: B0, a: N2, b: N2) extends WhereBase[S2, S0]
case class Where1_2(cond: B1, a: N2, b: N2) extends WhereBase[S2, S1]

