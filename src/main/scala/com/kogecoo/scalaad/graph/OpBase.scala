package com.kogecoo.scalaad.graph

import scala.language.higherKinds


abstract class UnaryOp_N[U[_], T] extends NonContainerNode[U, T]

abstract class UnaryOp_C[U[_], T] extends ContainerNode[U, T]

abstract class BinaryOp_NN[U[_], T] extends NonContainerNode[U, T]

abstract class BinaryOp_CN[U[_], T] extends ContainerNode[U, T]

abstract class BinaryOp_NC[U[_], T] extends ContainerNode[U, T]

abstract class BinaryOp_CC[U[_], T] extends ContainerNode[U, T]
