package com.kogecoo.scalaad

import com.kogecoo.scalaad.node.ValueTerm$

class ShapeCheckException(a: ValueTerm[_], b: ValueTerm[_], op: String)
  extends Exception(
    s"$op cannot applicable for variables with shape pair ${a.shape} and ${b.shape}"
  )
