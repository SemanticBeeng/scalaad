package com.kogecoo.scalaad.algorithm

import com.kogecoo.scalaad.Shape
import com.kogecoo.scalaad.graph._
import Predef.{any2stringadd => _, _}

import scala.language.higherKinds


class Grad(val grad: Map[Node[_ <: Shape], Node[_ <: Shape]]) {

  // FIXME: These are failed when grad made by order-n node with order-(m+k) adjoint.
  def apply(node: N0): Option[N0] = grad.get(node).map(_.asInstanceOf[N0])
  def apply(node: N1)(implicit d: DummyImplicit): Option[N1] = grad.get(node).map(_.asInstanceOf[N1])
  def apply(node: N2)(implicit d: DummyImplicit, d2: DummyImplicit): Option[N2] = grad.get(node).map(_.asInstanceOf[N2])

  // we cannot call eval with node which extracted by higher kind method.
  //def apply(node: Node[_ <: Shape]): Option[Node[_ <: Shape]] = grad.get(node)

}

object Grad {

  private[this] type S = Shape

  def apply(node: Node[_ <: S], grad: Node[_ <: S]): Grad = new Grad(Map(node -> grad))

  def empty: Grad = new Grad(Map.empty[Node[_ <: S], Node[_ <: S]])

  implicit class GradOp(val self: Grad) extends AnyVal {
    def ++(other: Grad): Grad = {
      val keys = self.grad.keySet | other.grad.keySet
      val (m: Map[Node[_ <: S], Node[_ <: S]]) = (for {
        k <- keys
        s =  self.grad.get(k)
        o =  other.grad.get(k)
        if s.isDefined || o.isDefined
      } yield {
        if (s.isDefined && o.isDefined) {
          (k, add(s.get, o.get))
        } else if (s.isDefined) {
          (k, s.get)
        } else {
          (k, o.get)
        }
      }).toMap
      new Grad(m)
    }

    def add(g1: Node[_ <: S], g2: Node[_ <: S]): Node[_ <: S] = {
      val n = (g1, g1.shape, g2, g2.shape) match {
        // instead of a.asInstanceOf[N0]
        case (a: N0 @unchecked, _: S0, b: N0 @unchecked, _: S0) => a  + b
        case (a: N0 @unchecked, _: S0, b: N1 @unchecked, _: S1) => a :+ b
        case (a: N0 @unchecked, _: S0, b: N2 @unchecked, _: S2) => a :+ b
        case (a: N1 @unchecked, _: S1, b: N0 @unchecked, _: S0) => a :+ b
        case (a: N1 @unchecked, _: S1, b: N1 @unchecked, _: S1) => a  + b
        case (a: N1 @unchecked, _: S1, b: N2 @unchecked, _: S2) => a :+ b
        case (a: N2 @unchecked, _: S2, b: N0 @unchecked, _: S0) => a :+ b
        case (a: N2 @unchecked, _: S2, b: N1 @unchecked, _: S1) => a :+ b
        case (a: N2 @unchecked, _: S2, b: N2 @unchecked, _: S2) => a  + b
      }
      n match {
        case n: Node[_] => n
      }
    }
  }

}


