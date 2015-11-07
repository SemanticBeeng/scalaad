package com.kogecoo.scalaad.graph


import scala.language.higherKinds


trait Node[T, Shape] {
  def eval[V](implicit E: Eval[Node[T, Shape], V]): V = E.eval(this)
  def forward[W, O](w: W)(implicit F: Forward[Node[T, Shape], W, O]): O = F.forward(this, w)
  def reverse[G, O](g: G)(implicit R: Reverse[Node[T, Shape], G, O]): O = R.reverse(this, g)
}

case class Const[T](v: T) extends N0[T]

case class Zeros[T](n: Int) extends N1[T]


trait Eval[N, V] {
  def eval(n: N): V
}

trait Forward[N, W, O] {
  def forward(n: N, wrt: W): O
}

trait Reverse[N, G, O] {
  def reverse(n: N, g: G): O
}

object Eval {
  def $00[T: Numeric](f: (Numeric[T], N0[T], N0[T]) => T): Eval[Op00[T], T] = new Eval[Op00[T], T] {
    def eval(n: Op00[T]): T = f(implicitly[Numeric[T]], n.left, n.right)
  }
  implicit def const[T] = new Eval[Const[T], T] {
    def eval(n: Const[T]): T = n.v
  }
  implicit def zeros[T] = new Eval[Zeros, Seq[T]] {
    def eval(n: Zeros): Seq[T] = Seq.fill(n.n)(implicitly[Numeric[T]].zero)
  }
  implicit def add00_[T: Numeric] = $00((N, l, r) => N.plus(l.eval, r.eval))
  implicit def add00[T : Numeric] = new Eval[Add00, T] {
    def eval(n: Add00): T = implicitly[Numeric[T]].plus(n.left.eval, n.right.eval)
  }
}

object Forward {
  implicit def add000[T] = new Forward[Add00, N0[T], N0[T]] {
    def forward(n: Add00, wrt: N0[T]) = Add00(n.left.forward(wrt), n.right.forward(wrt))
  }
}

object Reverse {

  /** short-handed writing method for reverse mode differentiation.
    * N: type of an operator Node
    * O: type of an output Node
    * G: type of a (root of) gradient tree which propagating from parent Node.
    */
  def $[N, G, O](f: (N, G) => O) = new Reverse[N, G, O] {
    def reverse(n: N, g: G): O = f(n, g)
  }

  /** more short-handed writing method for reverse mode differentiation of a binary operator.
    * left:  order-0 Node
    * right: order-0 Node
    * g:     order-0 Node
    */
  def $000[T, O](f: (N0[T], N0[T], N0[T]) => O): Reverse[Op00[T], N0[T], O] = new Reverse[Op00[T], N0[T], O] {
    def reverse(n: Op00[T], g: N0[T]): O = f(n.left, n.right, g)
  }

  /** more short-handed writing method for reverse mode differentiation of a binary operator.
    * left:  order-0 Node
    * right: order-0 Node
    * g:     order-1 Node
    */
  def $001[T, O](f: (N0[T], N0[T], N1[T]) => O): Reverse[Op00[T], N1[T], O] = new Reverse[Op00[T], N1[T], O] {
    def reverse(n: Op00[T], g: N1[T]): O = f(n.left, n.right, g)
  }

  /** more short-handed writing method for reverse mode differentiation of a binary operator.
    * left:  order-0 Node
    * right: order-0 Node
    * g:     order-2 Node
    */
  def $002[T, O](f: (N0[T], N0[T], N2[T]) => O): Reverse[Op00[T], N2[T], O] = new Reverse[Op00[T], N2[T], O] {
    def reverse(n: Op00[T], g: N2[T]): O = f(n.left, n.right, g)
  }

  /** more short-handed writing method for reverse mode differentiation of a binary operator.
    * left:  order-0 Node
    * right: order-1 Node
    * g:     order-0 Node
    */
  def $010[T, O](f: (N0[T], N1[T], N0[T]) => O): Reverse[Op01[T], N0[T], O] = new Reverse[Op01[T], N0[T], O] {
    def reverse(n: Op01[T], g: N0[T]): O = f(n.left, n.right, g)
  }

  /** more short-handed writing method for reverse mode differentiation of a binary operator.
    * left:  order-0 Node
    * right: order-1 Node
    * g:     order-1 Node
    */
  def $011[T, O](f: (N0[T], N1[T], N1[T]) => O): Reverse[Op01[T], N1[T], O] = new Reverse[Op01[T], N1[T], O] {
    def reverse(n: Op01[T], g: N1[T]): O = f(n.left, n.right, g)
  }

  /** more short-handed writing method for reverse mode differentiation of a binary operator.
    * left:  order-1 Node
    * right: order-0 Node
    * g:     order-0 Node
    */
  def $100[T, O](f: (N1[T], N0[T], N0[T]) => O): Reverse[Op10[T], N0[T], O] = new Reverse[Op10[T], N0[T], O] {
    def reverse(n: Op10[T], g: N0[T]): O = f(n.left, n.right, g)
  }

  /** more short-handed writing method for reverse mode differentiation of a binary operator.
    * left:  order-1 Node
    * right: order-0 Node
    * g:     order-1 Node
    */
  def $101[T, O](f: (N1[T], N0[T], N1[T]) => O): Reverse[Op10[T], N1[T], O] = new Reverse[Op10[T], N1[T], O] {
    def reverse(n: Op10[T], g: N1[T]): O = f(n.left, n.right, g)
  }

  /** more short-handed writing method for reverse mode differentiation of a binary operator.
    * left:  order-1 Node
    * right: order-0 Node
    * g:     order-0 Node
    */
  def $110[T, O](f: (N1[T], N1[T], N0[T]) => O): Reverse[Op11[T], N0[T], O] = new Reverse[Op11[T], N0[T], O] {
    def reverse(n: Op11[T], g: N0[T]): O = f(n.left, n.right, g)
  }

  /** more short-handed writing method for reverse mode differentiation of a binary operator.
    * left:  order-1 Node
    * right: order-1 Node
    * g:     order-1 Node
    */
  def $111[T, O](f: (N1[T], N1[T], N1[T]) => O): Reverse[Op11[T], N1[T], O] = new Reverse[Op11[T], N1[T], O] {
    def reverse(n: Op11[T], g: N1[T]): O = f(n.left, n.right, g)
  }

  /** more short-handed writing method for reverse mode differentiation of a binary operator.
    * left:  order-0 Node
    * right: order-2 Node
    * g:     order-0 Node
    */
  def $020[T, O](f: (N0[T], N2[T], N0[T]) => O): Reverse[Op02[T], N0[T], O] = new Reverse[Op02[T], N0[T], O] {
    def reverse(n: Op02[T], g: N0[T]): O = f(n.left, n.right, g)
  }

  /** more short-handed writing method for reverse mode differentiation of a binary operator.
    * left:  order-2 Node
    * right: order-0 Node
    * g:     order-0 Node
    */
  def $200[T, O](f: (N2[T], N0[T], N0[T]) => O): Reverse[Op20[T], N0[T], O] = new Reverse[Op20[T], N0[T], O] {
    def reverse(n: Op20[T], g: N0[T]): O = f(n.left, n.right, g)
  }

  /** more short-handed writing method for reverse mode differentiation of a binary operator.
    * left:  order-2 Node
    * right: order-0 Node
    * g:     order-0 Node
    */
  def $220[T, O](f: (N2[T], N2[T], N0[T]) => O): Reverse[Op22[T], N0[T], O] = new Reverse[Op22[T], N0[T], O] {
    def reverse(n: Op22[T], g: N0[T]): O = f(n.left, n.right, g)
  }

  // add
  implicit def add000[T] = $000((l, r, g) => Add00(l.reverse(g), r.reverse(g)))
  implicit def add001[T] = $001((l, r, g) => Add00(l.reverse(g), r.reverse(g)))
  implicit def add002[T] = $002((l, r, g) => Add00(l.reverse(g), r.reverse(g)))

  implicit def add010[T] = $010((l, r, g) => Add01(l.reverse(g), r.reverse(g)))
  implicit def add011[T] = $011((l, r, g) => Add01(l.reverse(g), r.reverse(g)))

  implicit def add100[T] = $100((l, r, g) => Add10(l.reverse(g), r.reverse(g)))
  implicit def add101[T] = $101((l, r, g) => Add10(l.reverse(g), r.reverse(g)))

  implicit def add110[T] = $110((l, r, g) => Add11(l.reverse(g), r.reverse(g)))
  implicit def add111[T] = $111((l, r, g) => Add11(l.reverse(g), r.reverse(g)))

  implicit def add020[T] = $020((l, r, g) => Add02(l.reverse(g), r.reverse(g)))

  implicit def add200[T] = $200((l, r, g) => Add20(l.reverse(g), r.reverse(g)))

  implicit def add220[T] = $220((l, r, g) => Add22(l.reverse(g), r.reverse(g)))

  // sub
  implicit def sub000[T] = $000((l, r, g) => Sub00(l.reverse(g), r.reverse(g)))
  implicit def sub001[T] = $001((l, r, g) => Sub00(l.reverse(g), r.reverse(g)))
  implicit def sub002[T] = $002((l, r, g) => Sub00(l.reverse(g), r.reverse(g)))

  implicit def sub010[T] = $010((l, r, g) => Sub01(l.reverse(g), r.reverse(g)))
  implicit def sub011[T] = $011((l, r, g) => Sub01(l.reverse(g), r.reverse(g)))

  implicit def sub100[T] = $100((l, r, g) => Sub10(l.reverse(g), r.reverse(g)))
  implicit def sub101[T] = $101((l, r, g) => Sub10(l.reverse(g), r.reverse(g)))

  implicit def sub110[T] = $110((l, r, g) => Sub11(l.reverse(g), r.reverse(g)))
  implicit def sub111[T] = $111((l, r, g) => Sub11(l.reverse(g), r.reverse(g)))

  implicit def sub020[T] = $020((l, r, g) => Sub02(l.reverse(g), r.reverse(g)))

  implicit def sub200[T] = $200((l, r, g) => Sub20(l.reverse(g), r.reverse(g)))

  implicit def sub220[T] = $220((l, r, g) => Sub22(l.reverse(g), r.reverse(g)))

  // mul
  implicit def mul000[T] = $000((l, r, g) => Mul00(l.reverse(g), r.reverse(g)))
  implicit def mul001[T] = $001((l, r, g) => Mul00(l.reverse(g), r.reverse(g)))
  implicit def mul002[T] = $002((l, r, g) => Mul00(l.reverse(g), r.reverse(g)))

  implicit def mul010[T] = $010((l, r, g) => Mul01(l.reverse(g), r.reverse(g)))
  implicit def mul011[T] = $011((l, r, g) => Mul01(l.reverse(g), r.reverse(g)))

  implicit def mul100[T] = $100((l, r, g) => Mul10(l.reverse(g), r.reverse(g)))
  implicit def mul101[T] = $101((l, r, g) => Mul10(l.reverse(g), r.reverse(g)))

  implicit def mul110[T] = $110((l, r, g) => Mul11(l.reverse(g), r.reverse(g)))
  implicit def mul111[T] = $111((l, r, g) => Mul11(l.reverse(g), r.reverse(g)))

  implicit def mul020[T] = $020((l, r, g) => Mul02(l.reverse(g), r.reverse(g)))

  implicit def mul200[T] = $200((l, r, g) => Mul20(l.reverse(g), r.reverse(g)))

  implicit def mul220[T] = $220((l, r, g) => Mul22(l.reverse(g), r.reverse(g)))

  // div
  implicit def div000[T] = $000((l, r, g) => Div00(l.reverse(g), r.reverse(g)))
  implicit def div001[T] = $001((l, r, g) => Div00(l.reverse(g), r.reverse(g)))
  implicit def div002[T] = $002((l, r, g) => Div00(l.reverse(g), r.reverse(g)))

  implicit def div010[T] = $010((l, r, g) => Div01(l.reverse(g), r.reverse(g)))
  implicit def div011[T] = $011((l, r, g) => Div01(l.reverse(g), r.reverse(g)))

  implicit def div100[T] = $100((l, r, g) => Div10(l.reverse(g), r.reverse(g)))
  implicit def div101[T] = $101((l, r, g) => Div10(l.reverse(g), r.reverse(g)))

  implicit def div110[T] = $110((l, r, g) => Div11(l.reverse(g), r.reverse(g)))
  implicit def div111[T] = $111((l, r, g) => Div11(l.reverse(g), r.reverse(g)))

  implicit def div020[T] = $020((l, r, g) => Div02(l.reverse(g), r.reverse(g)))

  implicit def div200[T] = $200((l, r, g) => Div20(l.reverse(g), r.reverse(g)))

  implicit def mul220[T] = $220((l, r, g) => Mul22(l.reverse(g), r.reverse(g)))

}

