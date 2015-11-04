# scalaad

Automatic differentiation for Scala

## Use

```scala
import com.kogecoo.scalaad.graph.Var  // always need to import
import com.kogecoo.scalaad.ScalarRule.Implicits._  // when x is a scalar variable

val x = Var(5.0)
val y = 2 * x + 3 * x * y

// forward-mode automatic differentiation
// partial differentiation w.r.t x
println(y.deriv(x))

// reverse-mode automatic differentiation computes a gradient
println(y.grad())

// we can get partial differentiation through `gradient` after running grad()
println(x.gradient)
println(y.gradient)
```

## TODO
* test
* maven repo
* compiler model
* high order differentiation
* Operators
  * where
  * dot
  * matmul
  * transform
  * max
  * min
  * abs
  * L-0 Norm
  * L-1 Norm
  * L-2 Norm

## Reference
* http://d.hatena.ne.jp/Nos/20130811/1376232751
* http://www.win-vector.com/dfiles/ReverseAccumulation.pdf
* http://www.win-vector.com/blog/2010/07/gradients-via-reverse-accumulation/
* http://www.win-vector.com/blog/2010/06/automatic-differentiation-with-scala/
* https://justindomke.wordpress.com/2009/02/17/automatic-differentiation-the-most-criminally-underused-tool-in-the-potential-machine-learning-toolbox/
* https://justindomke.wordpress.com/2009/03/24/a-simple-explanation-of-reverse-mode-automatic-differentiation/
* http://arxiv.org/pdf/1404.7456v1.pdf
* http://arxiv.org/pdf/1502.05767v2.pdf
* https://en.wikipedia.org/wiki/Automatic_differentiation
* http://www.met.reading.ac.uk/clouds/publications/adept.pdf
* http://colah.github.io/posts/2015-08-Backprop/index.html
* http://uhra.herts.ac.uk/bitstream/handle/2299/4335/903836.pdf?sequence=1

* http://arxiv.org/pdf/1509.07164v1.pdf
* Old and New Matrix Algebra Useful for Statistics, Thomas P. Minka, December 28, 2000
* http://research.microsoft.com/en-us/um/people/minka/papers/matrix/minka-matrix.pdf

* Matrix calculus
* https://en.wikipedia.org/wiki/Matrix_calculus
