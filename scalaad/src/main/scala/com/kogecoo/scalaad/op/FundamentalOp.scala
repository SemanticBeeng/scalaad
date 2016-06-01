package com.kogecoo.scalaad.op

import com.kogecoo.scalaad.S0
import com.kogecoo.scalaad.graph.ValueExpr


case object Add extends Op00 {

  def deriv: ValueExpr[S0] = {
  }

}

case object Sub extends Op00

case object Mul extends Op00

case object Div extends Op00

case object Pos extends Op0

case object Neg extends Op0

case object Identity extends Op0
