package com.kogecoo.scalaad.test.graph.op.spec

import com.kogecoo.scalaad.graph._
import com.kogecoo.scalaad.rule._
import com.kogecoo.scalaad.test.helper.gen._
import com.kogecoo.scalaad.test.helper.rule.SeqFloatSoftCompareRule
import com.kogecoo.scalaad.test.helper.rule.SeqFloatValueRule.Implicits._
import com.kogecoo.scalaad.test.helper.specgen.{UnaryOpSpec, UnaryOpExpectedBehaviorDef}

import org.scalacheck.Prop.forAll
import org.scalacheck.Properties

import scala.language.higherKinds


object TransposeSpecSeqFloat extends Properties("Transpose - Seq[Float]") {


  implicit val compareRule = new SeqFloatSoftCompareRule

  val nodeGen  = new SeqFloatNodeGen
  val valueGen = new SeqFloatValueGen
  val expects  = new TransposeSeqFloatExpectedBehavior

  val seqFloatSpecGen = new UnaryOpSpec[Seq, Float](expects, nodeGen, valueGen)

  property("a.T apply") = seqFloatSpecGen.applyScalar()
  property("a.T apply") = seqFloatSpecGen.applyContainer()
  property("a.T apply") = seqFloatSpecGen.applyVar()

  property("a.T (scalar) w.r.t. a")    = seqFloatSpecGen.derivScalarWrtSelf()
  property("a.T (scalar) w.r.t. b")    = seqFloatSpecGen.derivScalarWrtUnknown()
  property("a.T (container) w.r.t. a") = seqFloatSpecGen.derivContainerWrtSelf()
  property("a.T (container) w.r.t. b") = seqFloatSpecGen.derivContainerWrtUnknown()
  property("a.T (var) w.r.t. a")       = seqFloatSpecGen.derivContainerWrtSelf()
  property("a.T (var) w.r.t. b")       = seqFloatSpecGen.derivContainerWrtUnknown()

  property("a.T (scalar) propagete with value")        = seqFloatSpecGen.propagateScalarWithNCValue()
  property("a.T (scalar) propagate with container")    = seqFloatSpecGen.propagateScalarWithCValue()
  property("a.T (container) propagete with value")     = seqFloatSpecGen.propagateContainerWithNCValue()
  property("a.T (container) propagate with container") = seqFloatSpecGen.propagateContainerWithCValue()
  property("a.T (container) propagete with value")     = seqFloatSpecGen.propagateVarWithNCValue()
  property("a.T (container) propagate with container") = seqFloatSpecGen.propagateVarWithCValue()

  property("a.T grad")              = seqFloatSpecGen.gradScalar()
  property("a.T grad")              = seqFloatSpecGen.gradContainer()
  property("a.T grad")              = seqFloatSpecGen.gradVar()

  import com.kogecoo.scalaad.test.helper.matcher.ValueMatcherProp._

  property("a.T (scalar) deriv w.r.t. a.T") = forAll(nodeGen.genScalarConstWithSource()) {
    case a: ScalarConstSample[Seq, Float] =>
    val aT = a.node.T
    aT.deriv(aT) shouldBe 0f
  }

  property("a.T (container) deriv w.r.t. a.T") = forAll(nodeGen.genContainerConstWithSource()) {
    case a: ContainerConstSample[Seq, Float] =>
    val aT = a.node.T
    aT.deriv(aT) shouldBe Seq.fill(a.src.size)(0f)
  }

  property("a.T (var) deriv w.r.t. a.T") = forAll(nodeGen.genVarWithSource()) {
    case a: VarSample[Seq, Float] =>
    val aT = a.node.T
    aT.deriv(aT) shouldBe Seq.fill(a.src.size)(0f)
  }

}


class TransposeSeqFloatExpectedBehavior(implicit vr: ValueRule[Seq, Float]) extends UnaryOpExpectedBehaviorDef[Seq, Float] {

  override val zero: Float = 0f

  override def zero(shape: Seq[Float]): Seq[Float] = Seq.fill(shape.size)(0f)

  override def op(node: Node[Seq, Float]): Node[Seq, Float] = Transpose(node)

  override def applyScalar(a: Float): Float              = a
  override def applyContainer(a: Seq[Float]): Seq[Float] = a
  override def applyVar(a: Seq[Float]): Seq[Float]       = a

  override def derivVarWrtSelf(a: Seq[Float]): Seq[Float] = Seq.fill(a.size)(1f)

  override def propagateVarWithNCValue(a: Seq[Float], b: Float): Seq[Float] = Seq.fill(a.size)(1f * b)
  override def propagateVarWithCValue(a: Seq[Float], b: Seq[Float]): Seq[Float] = a.zip(b).map { case (_, y) => y }

  override def gradVar(a: Seq[Float]): Seq[Float] = Seq.fill(a.size)(1f)

}

