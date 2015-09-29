package com.kogecoo.scalaad

import com.kogecoo.scalaad.graph._
import com.kogecoo.scalaad.rule._

import org.nd4j.linalg.api.ndarray.INDArray
import org.nd4s.Implicits._

import scala.language.implicitConversions


object Nd4jRule {

  type T = Double
  type C = INDArray_[T]

  object Implicits {

    implicit val iNDArrayRuleRule = new INDArrayRule
    implicit val iNDArrayWrapperRule = new INDArrayWrapperRule

    // Literal conversion for constructing computational tree
    implicit def fromByte(v: Byte):     ScalarConst[INDArray_, Double] = ScalarConst[INDArray_, Double](v.toDouble)
    implicit def fromShort(v: Short):   ScalarConst[INDArray_, Double] = ScalarConst[INDArray_, Double](v.toDouble)
    implicit def fromInt(v: Int):       ScalarConst[INDArray_, Double] = ScalarConst[INDArray_, Double](v.toDouble)
    implicit def fromLong(v: Long):     ScalarConst[INDArray_, Double] = ScalarConst[INDArray_, Double](v.toDouble)
    implicit def fromFloat(v: Float):   ScalarConst[INDArray_, Double] = ScalarConst[INDArray_, Double](v.toDouble)
    implicit def fromDouble(v: Double): ScalarConst[INDArray_, Double] = ScalarConst[INDArray_, Double](v)

    implicit def fromINDArray(v: INDArray): ContainerConst[INDArray_, Double] = {
      ContainerConst[INDArray_, Double](INDArray_(v))
    }
  }

  // wrapper for INDArray
  case class INDArray_[T](data: INDArray) { type TypeOfScalarOp = T }

  class INDArrayRule extends INDArrayValueRule with INDArrayMathRule

  trait INDArrayValueRule extends ValueRule[INDArray_, Double] {
    override val zeroAdd: Value[INDArray_, Double] = toValue(0.0)
    override val zeroMul: Value[INDArray_, Double] = toValue(1.0)
    override val derivConst: Value[INDArray_, Double] = toValue(0.0)

    override def toValue(v: T): Value[INDArray_, T] = NonContainerValue[INDArray_, Double](v)
    override def toValue(v: C)(implicit e: DummyImplicit): Value[INDArray_, T] = {
      ContainerValue[INDArray_, Double](v)
    }

    override def addSS(l: C, r: C): C = INDArray_(l.data.add(r.data))
    override def subSS(l: C, r: C): C = INDArray_(l.data.sub(r.data))
    override def mulSS(l: C, r: C): C = INDArray_(l.data.mul(r.data))
    override def divSS(l: C, r: C): C = INDArray_(l.data.div(r.data))

    override def addSM(l: C, r: T): C = INDArray_(l.data.add(r))
    override def subSM(l: C, r: T): C = INDArray_(l.data.sub(r))
    override def mulSM(l: C, r: T): C = INDArray_(l.data.mul(r))
    override def divSM(l: C, r: T): C = INDArray_(l.data.div(r))

    override def addMS(l: T, r: C): C = INDArray_(r.data.add(l)) // it seems to be no radd in Nd4j
    override def subMS(l: T, r: C): C = INDArray_(r.data.rsub(l))
    override def mulMS(l: T, r: C): C = INDArray_(r.data.mul(l)) // it seems to be no rmul in Nd4j
    override def divMS(l: T, r: C): C = INDArray_(r.data.rdiv(l))

    override def addMM(l: T, r: T): T = l + r
    override def subMM(l: T, r: T): T = l - r
    override def mulMM(l: T, r: T): T = l * r
    override def divMM(l: T, r: T): T = l / r

    override def posS(v: C): C = INDArray_(v.data)
    override def negS(v: C): C = INDArray_(v.data.neg())

    override def posM(v: T): T = +v
    override def negM(v: T): T = -v
  }

  trait INDArrayMathRule extends MathRule[INDArray_, Double] {

    override def sinS(v: C): C = INDArray_(v.data.map(scala.math.sin))
    override def cosS(v: C): C = INDArray_(v.data.map(scala.math.cos))
    override def tanS(v: C): C = INDArray_(v.data.map(scala.math.tan))
    override def lnS(v: C):  C = INDArray_(v.data.map(scala.math.log))

    override def sinM(v: T): T = scala.math.sin(v)
    override def cosM(v: T): T = scala.math.cos(v)
    override def tanM(v: T): T = scala.math.tan(v)
    override def lnM(v: T):  T = scala.math.log(v)

  }

  class INDArrayWrapperRule extends ValueWrapperRule[INDArray, INDArray_, Double] {
    override def toWrapper(src: INDArray): INDArray_[Double] = INDArray_(src)
  }

}
