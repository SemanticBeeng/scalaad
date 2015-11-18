package com.kogecoo.scalaad.example

object Application {

  def main(args: Array[String]) = {
    scalarExample()
    breezeExample()
    nd4jExample()
  }

  def scalarExample() = {
    import com.kogecoo.scalaad.impl.std.Implicits._

    val x = Var(5.0)
    val y = Var(3.0)
    val z = x * sin(x) * 2 + y * x * 3

    println(z)
    println(z.deriv(x))  // forward-mode automatic differentiation
    println(z.deriv(y))

    println(z.grad())    // reverse-mode automatic differentiation 
    println(x.gradient)  // we can get partial differentiation through gradient after run grad()
    println(y.gradient)
  }

  def breezeExample() = {

    import com.kogecoo.scalaad.impl.breeze.Implicits._
    import breeze.linalg.DenseVector

    val x = Var(DenseVector(1.0, 2.0, 3.0))

    val y = 1.0 * sin(x) * 2 + x * 3

    println(y)
    println(y.deriv(x))

    println(y.grad())
    println(x.gradient)

  }

  def nd4jExample() = {
    import com.kogecoo.scalaad.impl.nd4j.Implicits._
    import org.nd4s.Implicits._

    val x = Var((1 to 9).asNDArray(1, 3))

    val y = 1 * sin(x) * 2 + x * 3

    println(y)
    println(y.deriv(x))

    println(y.grad())
    println(x.gradient)
  }

}
