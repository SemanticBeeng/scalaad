package com.kogecoo.scalaad.test.helper.impl.std

import com.kogecoo.scalaad.graph.{S1, S2}
import com.kogecoo.scalaad.{Shape1, Shape2, StdMat, StdVec}


object StdUtil {

  type T0  = Double
  type T1  = StdVec[T0]
  type T2  = StdMat[T0]

  final def shapeOf(a: T1): S1 = Shape1(a.size)
  final def shapeOf(a: T2): S2 = Shape2(a.size, a.head.size)

  final def shapeCheck1(a: T1, b: T1): Boolean = a.size == b.size
  final def shapeCheck2(a: T2, b: T2): Boolean = a.size == b.size && a.head.size == b.head.size

  final def columnWise(a: T2, b: T1, f: (T0, T0) => T0): T2 = {
    a.zip(b).map { case (x, y) => x.map(f(_, y)) }
  }

  final def rowWise(a: T2, b: T1, f: (T0, T0) => T0): T2 = {
    a.map { _.zip(b).map { case (x, y) => f(x, y) } }
  }


  // should move to std implicit
  final def const1(v: T0, s1: S1): T1 = Seq.fill(s1._1)(v)
  final def const2(v: T0, s2: S2): T2 = Seq.fill(s2._1, s2._2)(v)

  final def one0: T0 = 1.0
  final def zero0: T0 = 0.0

  final def diag(v: T0, size: Int): T2 = {
    val s = Seq.range(0, size)
    s.map { i => s.map { j => if (i == j) v else zero0 } }
  }

  final def diag(v: T1): T2 = {
    val s = v.indices
    s.map { i => s.map { j => if (i == j) v(i) else zero0 } }
  }

}
