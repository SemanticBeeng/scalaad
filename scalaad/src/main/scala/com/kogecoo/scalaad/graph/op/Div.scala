package com.kogecoo.scalaad.graph.op

import com.kogecoo.scalaad.graph._

import scala.language.higherKinds

// TODO: higher rank broadcasts
// case class Div0N
// case class DivN0
// case class DivNN


case class Div00[T](lhs: N0[T], rhs: N0[T]) extends N0[T] {
  val r = implicitly[ValueRule[U, T]]

  override def toString: String = s"(${ lhs.toString } / ${ rhs.toString })"

  override def apply(): T = r.div00(lhs(), rhs())

  override def deriv0      (wrt: N0[T]   ): N0[T]    = lhs.deriv0(wrt) / rhs.deriv0(wrt)
  override def deriv1[V[_]](wrt: N1[V, T]): N1[V, T] = lhs.deriv1(wrt) / rhs.deriv1(wrt)
  override def deriv2[M[_]](wrt: N2[M, T]): N2[M, T] = lhs.deriv2(wrt) / rhs.deriv2(wrt)

  override def propagate0      (g: N0[T]   ): N0[T]    = lhs.propagate0(g) / rhs.propagate0(g)
  override def propagate1[V[_]](g: N1[V, T]): N1[V, T] = lhs.propagate1(g) / rhs.propagate1(g)
  override def propagate2[M[_]](g: N2[M, T]): N2[M, T] = lhs.propagate2(g) / rhs.propagate2(g)
}


case class Div11[V[_], T](lhs: N1[V, T], rhs: N1[V, T]) extends N1[V, T] {
  val r = implicitly[ValueRule[V, T]]

  override def toString: String = s"(${ lhs.toString } / ${ rhs.toString })"

  override def apply(): T = r.div11(lhs(), rhs())

  override def deriv0      (wrt: N0[T]   ): N1[V, T] = lhs.deriv0(wrt)    / rhs.deriv0(wrt)
  override def deriv1[M[_]](wrt: N1[V, T]): N2[M, T] = lhs.deriv1[M](wrt) / rhs.deriv1[M](wrt)

  override def propagate0(g: N0[T]): N1[V, T]          = lhs.propagate0(g)    / rhs.propagate0(g)
  override def propagate1[M[_]](g: N1[V, T]): N2[M, T] = lhs.propagate1[M](g) / rhs.propagate1[M](g)
}


case class Div22[M[_], T](lhs: N2[M, T], rhs: N2[M, T]) extends N2[M, T] {
  val r = implicitly[ValueRule[M, T]]

  override def toString: String = s"(${ lhs.toString } / ${ rhs.toString })"

  override def apply(): M[T] = r.div22(lhs(), rhs())

  override def deriv0(wrt: N0[T]): N2[M, T] = lhs.deriv0(wrt) / rhs.deriv0(wrt)

  override def propagate0(g: N0[T]): N2[M, T] = lhs.propagate0(g) / rhs.propagate0(g)
}


case class BroadcastDiv01[V[_], T](lhs: N0[T], rhs: N1[V, T]) extends N1[V, T] {
  val r = implicitly[ValueRule[V, T]]

  override def toString: String = s"(${ lhs.toString } :/ ${ rhs.toString })"

  override def apply(): T = r.div01(lhs(), rhs())

  override def deriv0      (wrt: N0[T]   ): N1[V, T] = lhs.deriv0(wrt) :/ rhs.deriv0(wrt)
  override def deriv1[M[_]](wrt: N1[V, T]): N2[M, T] = lhs.deriv1(wrt) :/ rhs.deriv1[M](wrt)

  override def propagate0      (g: N0[T]   ): N1[V, T] = lhs.propagate0(g) :/ rhs.propagate0(g)
  override def propagate1[M[_]](g: N1[V, T]): N2[M, T] = lhs.propagate1(g) :/ rhs.propagate1[M](g)
}


case class BroadcastDiv10[V[_], T](lhs: N1[V, T], rhs: N0[T]) extends N1[V, T] {
  val r = implicitly[ValueRule[V, T]]

  override def toString: String = s"(${ lhs.toString } :/ ${ rhs.toString })"

  override def apply(): T = r.div10(lhs(), rhs())

  override def deriv0      (wrt: N0[T]   ): N1[V, T] = lhs.deriv0(wrt)    :/ rhs.deriv0(wrt)
  override def deriv1[M[_]](wrt: N1[V, T]): N2[M, T] = lhs.deriv1[M](wrt) :/ rhs.deriv1[V](wrt)

  override def propagate0      (g: N0[T]   ): N1[V, T] = lhs.propagate0(g)    :/ rhs.propagate0(g)
  override def propagate1[M[_]](g: N1[V, T]): N2[M, T] = lhs.propagate1[M](g) :/ rhs.propagate1[V](g)
}


case class BroadcastDiv12[M[_], V[_], T](lhs: N1[V, T], rhs: N2[M, T]) extends N2[M, T] {
  val r = implicitly[ValueRule[M, T]]

  override def toString: String = s"(${ lhs.toString } :/ ${ rhs.toString })"

  override def apply(): T = r.div12(lhs(), rhs())

  override def deriv0(wrt: N0[T]): N2[M, T] = lhs.deriv0(wrt) :/ rhs.deriv0(wrt)

  override def propagate0(g: N0[T]): N2[M, T] = lhs.propagate0(g) :/ rhs.propagate0(g)
}


case class BroadcastDiv21[M[_], V[_], T](lhs: N2[M, T], rhs: N1[V, T]) extends N2[M, T] {
  val r = implicitly[ValueRule[M, T]]

  override def toString: String = s"(${ lhs.toString } :/ ${ rhs.toString })"

  override def apply(): T = r.div21(lhs(), rhs())

  override def deriv0(wrt: N0[T]): N2[M, T] = lhs.deriv0(wrt) :/ rhs.deriv0(wrt)

  override def propagate0(g: N0[T]): N2[M, T] = lhs.propagate0(g) :/ rhs.propagate0(g)
}


case class BroadcastDiv02[M[_], T](lhs: N0[T], rhs: N2[M, T]) extends N2[M, T] {
  val r = implicitly[ValueRule[M, T]]

  override def toString: String = s"(${ lhs.toString } :/ ${ rhs.toString })"

  override def apply(): T = r.div02(lhs(), rhs())

  override def deriv0(wrt: N0[T]): N2[M, T] = lhs.deriv0(wrt) :/ rhs.deriv0(wrt)

  override def propagate0(g: N0[T]): N2[M, T] = lhs.propagate0(g) :/ rhs.propagate0(g)
}


case class BroadcastDiv20[M[_], T](lhs: N2[M, T], rhs: N0[T]) extends N2[M, T] {
  val r = implicitly[ValueRule[M, T]]

  override def toString: String = s"(${ lhs.toString } :/ ${ rhs.toString })"

  override def apply(): T = r.div20(lhs(), rhs())

  override def deriv0(wrt: N0[T]): N2[M, T] = lhs.deriv0(wrt) :/ rhs.deriv0(wrt)

  override def propagate0(g: N0[T]): N2[M, T] = lhs.propagate0(g) :/ rhs.propagate0(g)
}

