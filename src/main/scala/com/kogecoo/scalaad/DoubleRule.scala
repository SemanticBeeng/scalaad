package com.kogecoo.scalaad

import com.kogecoo.scalaad.graph.{Node, NonContainerVar, Scalar, ScalarConst}
import com.kogecoo.scalaad.rule._

import scala.language.implicitConversions

object DoubleRule {

  object Implicits {

    implicit val doubleRule = new DoubleRule
    implicit val doubleWrapperRule = new DoubleWrapperRule

    // Literal conversion for constructing computational tree
    implicit def fromByte(v: Byte):     ScalarConst[Scalar, Double] = ScalarConst[Scalar, Double](v.toDouble)
    implicit def fromShort(v: Short):   ScalarConst[Scalar, Double] = ScalarConst[Scalar, Double](v.toDouble)
    implicit def fromInt(v: Int):       ScalarConst[Scalar, Double] = ScalarConst[Scalar, Double](v.toDouble)
    implicit def fromLong(v: Long):     ScalarConst[Scalar, Double] = ScalarConst[Scalar, Double](v.toDouble)
    implicit def fromFloat(v: Float):   ScalarConst[Scalar, Double] = ScalarConst[Scalar, Double](v.toDouble)
    implicit def fromDouble(v: Double): ScalarConst[Scalar, Double] = ScalarConst[Scalar, Double](v)
  }

  class DoubleRule extends DoubleValueRule with DoubleMathRule

  trait DoubleValueRule extends ValueRule[Scalar, Double] {

    type T = Double
    type C = Scalar[Double]

    override val zeroAdd: Double = 0.0
    override val zeroMul: Double = 1.0

    override def zeroAdd(reference: C): C = Scalar(0.0)
    override def zeroMul(reference: C): C = Scalar(0.0)

    override def addSS(l: C, r: C): C = Scalar(l.data + r.data)
    override def subSS(l: C, r: C): C = Scalar(l.data - r.data)
    override def mulSS(l: C, r: C): C = Scalar(l.data * r.data)
    override def divSS(l: C, r: C): C = Scalar(l.data / r.data)

    override def addSM(l: C, r: T): C = Scalar(l.data + r)
    override def subSM(l: C, r: T): C = Scalar(l.data - r)
    override def mulSM(l: C, r: T): C = Scalar(l.data * r)
    override def divSM(l: C, r: T): C = Scalar(l.data / r)

    override def addMS(l: T, r: C): C = Scalar(l + r.data)
    override def subMS(l: T, r: C): C = Scalar(l - r.data)
    override def mulMS(l: T, r: C): C = Scalar(l * r.data)
    override def divMS(l: T, r: C): C = Scalar(l / r.data)

    override def addMM(l: T, r: T): T = l + r
    override def subMM(l: T, r: T): T = l - r
    override def mulMM(l: T, r: T): T = l * r
    override def divMM(l: T, r: T): T = l / r

    override def posS(v: C): C = Scalar(+v.data)
    override def negS(v: C): C = Scalar(-v.data)

    override def posM(v: T): T = +v
    override def negM(v: T): T = -v
  }

  trait DoubleMathRule extends MathRule[Scalar, Double] {

    type T = Double
    type C = Scalar[Double]

    override def sinS(v: C): C = Scalar(scala.math.sin(v.data))
    override def cosS(v: C): C = Scalar(scala.math.cos(v.data))
    override def tanS(v: C): C = Scalar(scala.math.tan(v.data))
    override def lnS(v: C):  C = Scalar(scala.math.log(v.data))
    override def expS(v: C):  C = Scalar(scala.math.exp(v.data))

    override def sinM(v: T): T = scala.math.sin(v)
    override def cosM(v: T): T = scala.math.cos(v)
    override def tanM(v: T): T = scala.math.tan(v)
    override def lnM(v: T):  T = scala.math.log(v)
    override def expM(v: T):  T = scala.math.exp(v)

  }

  class DoubleWrapperRule extends ValueWrapperRule[Double, Scalar, Double] {
    override def toVar(src: Double)(implicit r: ValueRule[Scalar, Double]): Node[Scalar, Double] = {
      new NonContainerVar[Scalar, Double](src)
    }
  }

}
