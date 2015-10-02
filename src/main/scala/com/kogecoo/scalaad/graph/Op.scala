package com.kogecoo.scalaad.graph

import Predef.{ any2stringadd => _, _ }
import com.kogecoo.scalaad.rule.ValueRule
import com.kogecoo.scalaad.rule.Implicits._
import scala.language.higherKinds


case class Add_CC[U[_], T](lhs: ContainerNode[U, T], rhs: ContainerNode[U, T])(implicit vr: ValueRule[U, T]) extends BinaryOp_CC[U, T] {
  override def toString: String = s"(${ lhs.toString } + ${ rhs.toString })"
  override def apply(): U[T] = lhs() + rhs()
  override def deriv(wrt: Node[U, T]): U[T] = lhs.deriv(wrt) + rhs.deriv(wrt)
  override def propagate(g: U[T]): U[T] = lhs.propagate(g) + rhs.propagate(g)
}

case class Add_CN[U[_], T](lhs: ContainerNode[U, T], rhs: NonContainerNode[U, T])(implicit vr: ValueRule[U, T]) extends BinaryOp_CN[U, T] {
  override def toString: String = s"(${ lhs.toString } + ${ rhs.toString })"
  override def apply(): U[T] = lhs() + rhs()
  override def deriv(wrt: Node[U, T]): U[T] = lhs.deriv(wrt) + rhs.deriv(wrt)
  override def propagate(g: U[T]): U[T] = lhs.propagate(g)
}

case class Add_NC[U[_], T](lhs: NonContainerNode[U, T], rhs: ContainerNode[U, T])(implicit vr: ValueRule[U, T]) extends BinaryOp_NC[U, T] {
  override def toString: String = s"(${ lhs.toString } + ${ rhs.toString })"
  override def apply(): U[T] = lhs() + rhs()
  override def deriv(wrt: Node[U, T]): U[T] = lhs.deriv(wrt) + rhs.deriv(wrt)
  override def propagate(g: U[T]): U[T] = rhs.propagate(g)
}

case class Add_NN[U[_], T](lhs: NonContainerNode[U, T], rhs: NonContainerNode[U, T])(implicit vr: ValueRule[U, T]) extends BinaryOp_NN[U, T] {
  override def toString: String = s"(${ lhs.toString } + ${ rhs.toString })"
  override def apply(): T = lhs() + rhs()
  override def deriv(wrt: Node[U, T]): T = lhs.deriv(wrt) + rhs.deriv(wrt)
  override def propagate(g: T): T = lhs.propagate(g) + rhs.propagate(g)
}


case class Sub_CC[U[_], T](lhs: ContainerNode[U, T], rhs: ContainerNode[U, T])(implicit vr: ValueRule[U, T]) extends BinaryOp_CC[U, T] {
  override def toString: String = s"(${ lhs.toString } - ${ rhs.toString })"
  override def apply(): U[T] = lhs() - rhs()
  override def deriv(wrt: Node[U, T]): U[T] = lhs.deriv(wrt) - rhs.deriv(wrt)
  override def propagate(g: U[T]): U[T] = lhs.propagate(g) + rhs.propagate(vr.negS(g))
}

case class Sub_CN[U[_], T](lhs: ContainerNode[U, T], rhs: NonContainerNode[U, T])(implicit vr: ValueRule[U, T]) extends BinaryOp_CN[U, T] {
  override def toString: String = s"(${ lhs.toString } - ${ rhs.toString })"
  override def apply(): U[T] = lhs() - rhs()
  override def deriv(wrt: Node[U, T]): U[T] = lhs.deriv(wrt) - rhs.deriv(wrt)
  override def propagate(g: U[T]): U[T] = lhs.propagate(g)
}

case class Sub_NC[U[_], T](lhs: NonContainerNode[U, T], rhs: ContainerNode[U, T])(implicit vr: ValueRule[U, T]) extends BinaryOp_NC[U, T] {
  override def toString: String = s"(${ lhs.toString } - ${ rhs.toString })"
  override def apply(): U[T] = lhs() - rhs()
  override def deriv(wrt: Node[U, T]): U[T] = lhs.deriv(wrt) - rhs.deriv(wrt)
  override def propagate(g: U[T]): U[T] = rhs.propagate(vr.negS(g))
}

case class Sub_NN[U[_], T](lhs: NonContainerNode[U, T], rhs: NonContainerNode[U, T])(implicit vr: ValueRule[U, T]) extends BinaryOp_NN[U, T] {
  override def toString: String = s"(${ lhs.toString } - ${ rhs.toString })"
  override def apply(): T = lhs() - rhs()
  override def deriv(wrt: Node[U, T]): T = lhs.deriv(wrt) - rhs.deriv(wrt)
  override def propagate(g: T): T = lhs.propagate(g) + rhs.propagate(vr.negM(g))
}

case class Mul_CC[U[_], T](lhs: ContainerNode[U, T], rhs: ContainerNode[U, T])(implicit vr: ValueRule[U, T]) extends BinaryOp_CC[U, T] {
  override def toString: String = s"(${ lhs.toString } * ${ rhs.toString })"
  override def apply(): U[T] = lhs() * rhs()
  override def deriv(wrt: Node[U, T]): U[T] = {
    lhs.deriv(wrt) * rhs() + lhs() * rhs.deriv(wrt)
  }

  override def propagate(g: U[T]): U[T] = {
    lhs.propagate(g * rhs()) + rhs.propagate(g * lhs())
  }
}

case class Mul_CN[U[_], T](lhs: ContainerNode[U, T], rhs: NonContainerNode[U, T])(implicit vr: ValueRule[U, T]) extends BinaryOp_CN[U, T] {
  override def toString: String = s"(${ lhs.toString } * ${ rhs.toString })"
  override def apply(): U[T] = lhs() * rhs()
  override def deriv(wrt: Node[U, T]): U[T] = {
    lhs.deriv(wrt) * rhs() + lhs() * rhs.deriv(wrt)
  }

  override def propagate(g: U[T]): U[T] = {
    lhs.propagate(g * rhs())
  }
}

case class Mul_NC[U[_], T](lhs: NonContainerNode[U, T], rhs: ContainerNode[U, T])(implicit vr: ValueRule[U, T]) extends BinaryOp_NC[U, T] {
  override def toString: String = s"(${ lhs.toString } * ${ rhs.toString })"
  override def apply(): U[T] = lhs() * rhs()
  override def deriv(wrt: Node[U, T]): U[T] = {
    lhs.deriv(wrt) * rhs() + lhs() * rhs.deriv(wrt)
  }

  override def propagate(g: U[T]): U[T] = {
    rhs.propagate(g * lhs())
  }
}
case class Mul_NN[U[_], T](lhs: NonContainerNode[U, T], rhs: NonContainerNode[U, T])(implicit vr: ValueRule[U, T]) extends BinaryOp_NN[U, T] {
  override def toString: String = s"(${ lhs.toString } * ${ rhs.toString })"
  override def apply(): T = lhs() * rhs()
  override def deriv(wrt: Node[U, T]): T = {
    lhs.deriv(wrt) * rhs() + lhs() * rhs.deriv(wrt)
  }

  override def propagate(g: T): T = {
    lhs.propagate(g * rhs()) + rhs.propagate(g * lhs())
  }
}

case class Div_CC[U[_], T](lhs: ContainerNode[U, T], rhs: ContainerNode[U, T])(implicit vr: ValueRule[U, T]) extends BinaryOp_CC[U, T] {
  override def toString: String = s"(${ lhs.toString } / ${ rhs.toString })"
  override def apply(): U[T] = lhs() / rhs()
  override def deriv(wrt: Node[U, T]): U[T] = {
    val rhs_val = rhs()
    val num = lhs.deriv(wrt) * rhs_val - lhs() * rhs.deriv(wrt)
    val den = rhs_val * rhs_val
    num / den
  }

  override def propagate(g: U[T]): U[T] = {
    val rhs_val = rhs()
    lhs.propagate(g / rhs_val) + rhs.propagate(vr.negS(g) * lhs() / rhs_val)
  }
}
case class Div_CN[U[_], T](lhs: ContainerNode[U, T], rhs: NonContainerNode[U, T])(implicit vr: ValueRule[U, T]) extends BinaryOp_CN[U, T] {
  override def toString: String = s"(${ lhs.toString } / ${ rhs.toString })"
  override def apply(): U[T] = lhs() / rhs()
  override def deriv(wrt: Node[U, T]): U[T] = {
    val rhs_val = rhs()
    val num = lhs.deriv(wrt) * rhs_val - lhs() * rhs.deriv(wrt)
    val den = rhs_val * rhs_val
    num / den
  }

  override def propagate(g: U[T]): U[T] = {
    val rhs_val = rhs()
    lhs.propagate(g / rhs_val)
  }
}
case class Div_NC[U[_], T](lhs: NonContainerNode[U, T], rhs: ContainerNode[U, T])(implicit vr: ValueRule[U, T]) extends BinaryOp_NC[U, T] {
  override def toString: String = s"(${ lhs.toString } / ${ rhs.toString })"
  override def apply(): U[T] = lhs() / rhs()
  override def deriv(wrt: Node[U, T]): U[T] = {
    val rhs_val = rhs()
    val num = lhs.deriv(wrt) * rhs_val - lhs() * rhs.deriv(wrt)
    val den = rhs_val * rhs_val
    num / den
  }

  override def propagate(g: U[T]): U[T] = {
    val rhs_val = rhs()
    rhs.propagate(vr.negS(g) * lhs() / rhs_val)
  }
}

case class Div_NN[U[_], T](lhs: NonContainerNode[U, T], rhs: NonContainerNode[U, T])(implicit vr: ValueRule[U, T]) extends BinaryOp_NN[U, T] {
  override def toString: String = s"(${ lhs.toString } / ${ rhs.toString })"
  override def apply(): T = lhs() / rhs()
  override def deriv(wrt: Node[U, T]): T = {
    val rhs_val = rhs()
    val num = lhs.deriv(wrt) * rhs_val - lhs() * rhs.deriv(wrt)
    val den = rhs_val * rhs_val
    num / den
  }

  override def propagate(g: T): T = {
    val rhs_val = rhs()
    lhs.propagate(g / rhs_val) + rhs.propagate(vr.negM(g) * lhs() / rhs_val)
  }
}

case class Pos_C[U[_], T](n: ContainerNode[U, T])(implicit vr: ValueRule[U, T]) extends UnaryOp_C[U, T] {
  override def toString: String = s"+(${ n })"
  override def apply(): U[T] = vr.posS(n())
  override def deriv(wrt: Node[U, T]): U[T] = vr.posS(n.deriv(wrt))
  override def propagate(g: U[T]): U[T] = n.propagate(g)
}

case class Pos_N[U[_], T](n: NonContainerNode[U, T])(implicit vr: ValueRule[U, T]) extends UnaryOp_N[U, T] {
  override def toString: String = s"+(${ n })"
  override def apply(): T = vr.posM(n())
  override def deriv(wrt: Node[U, T]): T = vr.posM(n.deriv(wrt))
  override def propagate(g: T): T = n.propagate(g)
}

case class Neg_C[U[_], T](n: ContainerNode[U, T])(implicit vr: ValueRule[U, T]) extends UnaryOp_C[U, T] {
  override def toString: String = s"-(${ n })"
  override def apply(): U[T] = vr.negS(n())
  override def deriv(wrt: Node[U, T]): U[T] = vr.negS(n.deriv(wrt))
  override def propagate(g: U[T]): U[T] = n.propagate(vr.negS(g))
}

case class Neg_N[U[_], T](n: NonContainerNode[U, T])(implicit vr: ValueRule[U, T]) extends UnaryOp_N[U, T] {
  override def toString: String = s"-(${ n })"
  override def apply(): T = vr.negM(n())
  override def deriv(wrt: Node[U, T]): T = vr.negM(n.deriv(wrt))
  override def propagate(g: T): T = n.propagate(vr.negM(g))
}

