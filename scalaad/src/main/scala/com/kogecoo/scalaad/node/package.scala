package com.kogecoo.scalaad

import Predef.{ any2stringadd => _ }


package object node {

  type S0 = Shape0
  type S1 = Shape1
  type S2 = Shape2

  type N0 = ValueTerm[S0]
  type N1 = ValueTerm[S1]
  type N2 = ValueTerm[S2]

  type B0 = BooleanTerm[S0]
  type B1 = BooleanTerm[S1]
  type B2 = BooleanTerm[S2]

}
