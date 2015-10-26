package com.kogecoo.scalaad.test.graph.math.spec

import com.kogecoo.scalaad.graph.{acos, cos, Node}
import com.kogecoo.scalaad.rule._
import com.kogecoo.scalaad.test.helper.gen._
import com.kogecoo.scalaad.test.helper.rule.SeqFloatSoftCompareRule
import com.kogecoo.scalaad.test.helper.rule.SeqFloatValueRule.Implicits._
import com.kogecoo.scalaad.test.helper.specgen.{UnaryOpSpec, UnaryOpExpectedBehaviorDef}
import org.scalacheck.Properties

import scala.language.higherKinds


object AcosSpecSeqFloat extends Properties("Acos - Seq[Float]") {

  implicit val compareRule = new SeqFloatSoftCompareRule

  val nodeGen  = new SeqFloatNodeGenWithValueRange(-1f, 1f)
  val valueGen = new SeqFloatValueGenWithValueRange(-1f, 1f)
  val expects  = new AcosSeqFloatExpectedBehavior

  val seqFloatSpecGen = new UnaryOpSpec[Seq, Float](expects, nodeGen, valueGen)

  property("acos(a) apply") = seqFloatSpecGen.applyScalar()
  property("acos(a) apply") = seqFloatSpecGen.applyContainer()
  property("acos(a) apply") = seqFloatSpecGen.applyVar()

  property("acos(a) (scalar) w.r.t. a")    = seqFloatSpecGen.derivScalarWrtSelf()
  property("acos(a) (scalar) w.r.t. b")    = seqFloatSpecGen.derivScalarWrtUnknown()
  property("acos(a) (container) w.r.t. a") = seqFloatSpecGen.derivContainerWrtSelf()
  property("acos(a) (container) w.r.t. b") = seqFloatSpecGen.derivContainerWrtUnknown()
  property("acos(a) (var) w.r.t. a")       = seqFloatSpecGen.derivContainerWrtSelf()
  property("acos(a) (var) w.r.t. b")       = seqFloatSpecGen.derivContainerWrtUnknown()

  property("acos(a) (scalar) propagete with value")        = seqFloatSpecGen.propagateScalarWithNCValue()
  property("acos(a) (scalar) propagate with container")    = seqFloatSpecGen.propagateScalarWithCValue()
  property("acos(a) (container) propagete with value")     = seqFloatSpecGen.propagateContainerWithNCValue()
  property("acos(a) (container) propagate with container") = seqFloatSpecGen.propagateContainerWithCValue()
  property("acos(a) (container) propagete with value")     = seqFloatSpecGen.propagateVarWithNCValue()
  property("acos(a) (container) propagate with container") = seqFloatSpecGen.propagateVarWithCValue()

  property("acos(a) grad")              = seqFloatSpecGen.gradScalar()
  property("acos(a) grad")              = seqFloatSpecGen.gradContainer()
  property("acos(a) grad")              = seqFloatSpecGen.gradVar()

}


class AcosSeqFloatExpectedBehavior(implicit vr: MathRule[Seq, Float]) extends UnaryOpExpectedBehaviorDef[Seq, Float] {

  override val zero: Float = 0f

  override def zero(shape: Seq[Float]): Seq[Float] = Seq.fill(shape.size)(0f)

  override def op(node: Node[Seq, Float]): Node[Seq, Float] = acos(node)

  override def applyScalar(a: Float): Float              = scala.math.acos(a).toFloat
  override def applyContainer(a: Seq[Float]): Seq[Float] = a.map { x => scala.math.acos(x.toDouble).toFloat }
  override def applyVar(a: Seq[Float]): Seq[Float]       = a.map { x => scala.math.acos(x.toDouble).toFloat }

  override def derivVarWrtSelf(a: Seq[Float]): Seq[Float] =  a.map { x => -1f / scala.math.sqrt(1f - x * x).toFloat }

  override def propagateVarWithNCValue(a: Seq[Float], b: Float): Seq[Float] = a.map { x => -b / scala.math.sqrt(1f - x * x).toFloat }

  override def propagateVarWithCValue(a: Seq[Float], b: Seq[Float]): Seq[Float] = a.zip(b).map { case (x, y) =>
    -y / scala.math.sqrt(1f - x * x).toFloat
  }

  override def gradVar(a: Seq[Float]): Seq[Float] = a.map { x => -1f / scala.math.sqrt(1f - x * x).toFloat }

}
