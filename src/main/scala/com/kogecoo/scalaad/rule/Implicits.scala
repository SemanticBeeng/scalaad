package com.kogecoo.scalaad.rule

import scala.language.higherKinds


object Implicits {

  implicit class ValueOps[U[_], T](val self: U[T]) extends AnyVal {

    def +(rhs: U[T])(implicit vr: ValueRule[U, T]): U[T] = vr.addSS(self, rhs)
    def -(rhs: U[T])(implicit vr: ValueRule[U, T]): U[T] = vr.subSS(self, rhs)
    def *(rhs: U[T])(implicit vr: ValueRule[U, T]): U[T] = vr.mulSS(self, rhs)
    def /(rhs: U[T])(implicit vr: ValueRule[U, T]): U[T] = vr.divSS(self, rhs)

    def +(rhs: T)(implicit vr: ValueRule[U, T]): U[T] = vr.addSM(self, rhs)
    def -(rhs: T)(implicit vr: ValueRule[U, T]): U[T] = vr.subSM(self, rhs)
    def *(rhs: T)(implicit vr: ValueRule[U, T]): U[T] = vr.mulSM(self, rhs)
    def /(rhs: T)(implicit vr: ValueRule[U, T]): U[T] = vr.divSM(self, rhs)

    def unary_+()(implicit vr: ValueRule[U, T]): U[T] = vr.posS(self)
    def unary_-()(implicit vr: ValueRule[U, T]): U[T] = vr.negS(self)

  }

  implicit class ValueOps2[U[_], T](val self: T) extends AnyVal {

    def +(rhs: U[T])(implicit vr: ValueRule[U, T]): U[T] = vr.addMS(self, rhs)
    def -(rhs: U[T])(implicit vr: ValueRule[U, T]): U[T] = vr.subMS(self, rhs)
    def *(rhs: U[T])(implicit vr: ValueRule[U, T]): U[T] = vr.mulMS(self, rhs)
    def /(rhs: U[T])(implicit vr: ValueRule[U, T]): U[T] = vr.divMS(self, rhs)

    def +(rhs: T)(implicit vr: ValueRule[U, T]): T = vr.addMM(self, rhs)
    def -(rhs: T)(implicit vr: ValueRule[U, T]): T = vr.subMM(self, rhs)
    def *(rhs: T)(implicit vr: ValueRule[U, T]): T = vr.mulMM(self, rhs)
    def /(rhs: T)(implicit vr: ValueRule[U, T]): T = vr.divMM(self, rhs)

    def unary_+()(implicit vr: ValueRule[U, T], d: DummyImplicit): T = vr.posM(self)
    def unary_-()(implicit vr: ValueRule[U, T], d: DummyImplicit): T = vr.negM(self)

  }

}
