package com.kogecoo.scalaad.algorithm

import com.kogecoo.scalaad.graph._

import scala.Predef.{any2stringadd => _}


trait Reverse[N, G] {

  def reverse(n: N, g: G): Grad

}

object Reverse {

  // target Node, propagated grad
  // 0 0
  // 0 1
  // 0 2
  // 1 0
  // 1 1
  // 1 2
  // 2 0
  // 2 1
  // 2 2

  implicit def reverse00: Reverse[N0, N0] = new Reverse[N0, N0] {

    private[this] type N = N0
    private[this] type G = N0

    def reverse(n: N, g: G): Grad = n match {
      // Leaf nodes
      case _: Var0   => Grad(n, g * One0())
      case _: Zero0  => Grad.empty
      case _: One0   => Grad.empty
      case _: Half0  => Grad.empty
      case _: Const0 => Grad.empty

      // Unary ops
      case Pos0(v) => v.reverse[G](+g)
      case Neg0(v) => v.reverse[G](-g)

      // Binary ops
      case Add00(l, r) => l.reverse[G](g) ++ r.reverse[G](g)
      case Sub00(l, r) => l.reverse[G](g) ++ r.reverse[G](-g)
      case Mul00(l, r) => l.reverse[G](g * r) ++ r.reverse[G](l * g)
      case Div00(l, r) => l.reverse[G](g / r) ++ r.reverse[G](l * g / r / r)

      // Math
      case Sin0(v) => v.reverse[G](g * Cos0(v))
      case Cos0(v) => v.reverse[G](-g * Sin0(v))
      case Tan0(v) => v.reverse[G](g * (One0() + (Tan0(v) * Tan0(v))))

      case Asin0(v) => v.reverse[G](g *  (One0() / Sqrt0(One0() - (v * v))))
      case Acos0(v) => v.reverse[G](g * -(One0() / Sqrt0(One0() - (v * v))))
      case Atan0(v) => v.reverse[G](g * -(One0() / (One0() + (v * v))))

      case Sinh0(v) => v.reverse[G](g * Cosh0(v))
      case Cosh0(v) => v.reverse[G](g * Sinh0(v))
      case Tanh0(v) => v.reverse[G](g * (One0() - (Tanh0(v) * Tanh0(v))))

      case Ln0(v)      => v.reverse[G](g / v)
      case Exp0(v)     => v.reverse[G](g * Exp0(v))
      case Sqrt0(v)    => v.reverse[G](g * (Half0() / Sqrt0(v)))
      case Pow00(l, r) => {
        val lg = l.reverse[G](g * r * Pow00(l, r - One0()))
        val rg = r.reverse[G](g * Ln0(l) * Pow00(l, r))
        lg ++ rg
      }

      // Experimental
      case Abs0(v)     => Where0_0(Gt00(v, Zero0()), v, -v).reverse(g)
      case Max00(l, r) => Where0_0(Gt00(l, r), l, r).reverse(g)
      case Min00(l, r) => Where0_0(Lt00(l, r), l, r).reverse(g)
    }
  }

  implicit def reverse01: Reverse[N0, N1] = new Reverse[N0, N1] {

    private[this] type N = N0
    private[this] type G = N1

    def reverse(n: N, g: G): Grad = n match {

      // Leaf nodes
      case _: Var0   => Grad(n, g :* One0())
      case _: Zero0  => Grad.empty
      case _: Half0  => Grad.empty
      case _: One0   => Grad.empty
      case _: Const0 => Grad.empty

      // Unary ops
      case Pos0(v) => v.reverse[G](+g)
      case Neg0(v) => v.reverse[G](-g)

      // Binary ops
      case Add00(l, r) => l.reverse[G](g) ++ r.reverse[G](g)
      case Sub00(l, r) => l.reverse[G](g) ++ r.reverse[G](-g)
      case Mul00(l, r) => l.reverse[G](g :* r) ++ r.reverse[G](l :* g)
      case Div00(l, r) => l.reverse[G](g :/ r) ++ r.reverse[G](l :* g :/ r :/ r)

      // Math
      case Sin0(v) => v.reverse[G](g :* Cos0(v))
      case Cos0(v) => v.reverse[G](-g :* Sin0(v))
      case Tan0(v) => v.reverse[G](g :* (One0() + (Tan0(v) * Tan0(v))))

      case Asin0(v) => v.reverse[G](g :*  (One0() / Sqrt0(One0() - (v * v))))
      case Acos0(v) => v.reverse[G](g :* -(One0() / Sqrt0(One0() - (v * v))))
      case Atan0(v) => v.reverse[G](g :* -(One0() / (One0() + (v * v))))

      case Sinh0(v) => v.reverse[G](g :* Cosh0(v))
      case Cosh0(v) => v.reverse[G](g :* Sinh0(v))
      case Tanh0(v) => v.reverse[G](g :* (One0() - (Tanh0(v) * Tanh0(v))))

      case Ln0(v)      => v.reverse[G](g :/ v)
      case Exp0(v)     => v.reverse[G](g :* Exp0(v))
      case Sqrt0(v)    => v.reverse[G](g :* (Half0() / Sqrt0(v)))
      case Pow00(l, r) => {
        val lhs = l.reverse[G](g :* r :* Pow00(l, r - One0()))
        val rhs = r.reverse[G](g :* Ln0(l) * Pow00(l, r))
        lhs ++ rhs
      }

      // Experimental
      case Abs0(v)     => Where0_0(Gt00(v, Zero0()), v, -v).reverse(g)
      case Max00(l, r) => Where0_0(Gt00(l, r), l, r).reverse(g)
      case Min00(l, r) => Where0_0(Lt00(l, r), l, r).reverse(g)
    }
  }

  implicit def reverse02: Reverse[N0, N2] = new Reverse[N0, N2] {

    private[this] type N  = N0
    private[this] type G  = N2

    def reverse(n: N, g: G): Grad = n match {

      // Leaf nodes
      case _: Var0   => Grad(n, g :* One0())
      case _: Zero0  => Grad.empty
      case _: Half0  => Grad.empty
      case _: One0   => Grad.empty
      case _: Const0 => Grad.empty

      // Unary ops
      case Pos0(v) => v.reverse[G](+g)
      case Neg0(v) => v.reverse[G](-g)

      // Binary ops
      case Add00(l, r) => l.reverse[G](g) ++ r.reverse[G](g)
      case Sub00(l, r) => l.reverse[G](g) ++ r.reverse[G](-g)
      case Mul00(l, r) => l.reverse[G](g :* r) ++ r.reverse[G](l :* g)
      case Div00(l, r) => l.reverse[G](g :/ r) ++ r.reverse[G](l :* g :/ r :/ r)

      // Math
      case Sin0(v) => v.reverse[G](g :* Cos0(v))
      case Cos0(v) => v.reverse[G](-g :* Sin0(v))
      case Tan0(v) => v.reverse[G](g :* (One0() + (Tan0(v) * Tan0(v))))

      case Asin0(v) => v.reverse[G](g :*  (One0() / Sqrt0(One0() - (v * v))))
      case Acos0(v) => v.reverse[G](g :* -(One0() / Sqrt0(One0() - (v * v))))
      case Atan0(v) => v.reverse[G](g :* -(One0() / (One0() + (v * v))))

      case Sinh0(v) => v.reverse[G](g :* Cosh0(v))
      case Cosh0(v) => v.reverse[G](g :* Sinh0(v))
      case Tanh0(v) => v.reverse[G](g :* (One0() - (Tanh0(v) * Tanh0(v))))

      case Ln0(v)      => v.reverse[G](g :/ v)
      case Exp0(v)     => v.reverse[G](g :* Exp0(v))
      case Sqrt0(v)    => v.reverse[G](g :* (Half0() / Sqrt0(v)))
      case Pow00(l, r) => {
        val lhs = l.reverse[G](g :* r :* Pow00(l, r - One0()))
        val rhs = r.reverse[G](g :* Ln0(l) * Pow00(l, r))
        lhs ++ rhs
      }

      // Experimental
      case Abs0(v)     => Where0_0(Gt00(v, Zero0()), v, -v).reverse(g)
      case Max00(l, r) => Where0_0(Gt00(l, r), l, r).reverse(g)
      case Min00(l, r) => Where0_0(Lt00(l, r), l, r).reverse(g)
    }
  }

  implicit def reverse10: Reverse[N1, N0] = new Reverse[N1, N0] {

    private[this] type N  = N1
    private[this] type G0  = N0
    private[this] type G1 = N1

    def reverse(n: N, g: G0): Grad = n match {

      // Leaf nodes
      case Var1(_, shape)   => Grad(n, g * One0())
      case Zero1(shape)     => Grad.empty
      case Half1(shape)     => Grad.empty
      case One1(shape)      => Grad.empty
      case Const1(_, shape) => Grad.empty

      // Unary ops
      case Pos1(v) => v.reverse[G0](+g)
      case Neg1(v) => v.reverse[G0](-g)
      case Transpose1(v) => v.reverse[G0](g)

      // Binary ops
      case Add01(l, r) => l.reverse[G0](g) ++ r.reverse[G0](g)
      case Add10(l, r) => l.reverse[G0](g) ++ r.reverse[G0](g)
      case Add11(l, r) => l.reverse[G0](g) ++ r.reverse[G0](g)

      case Sub01(l, r) => l.reverse[G0](g) ++ r.reverse[G0](-g)
      case Sub10(l, r) => l.reverse[G0](g) ++ r.reverse[G0](-g)
      case Sub11(l, r) => l.reverse[G0](g) ++ r.reverse[G0](-g)

      case Mul01(l, r) => l.reverse[G1](g :* r) ++ r.reverse[G0](l  * g)
      case Mul10(l, r) => l.reverse[G0](g  * r) ++ r.reverse[G1](l :* g)
      case Mul11(l, r) => l.reverse[G1](g :* r) ++ r.reverse[G1](l :* g)
      case Div01(l, r) => l.reverse[G1](g :/ r) ++ r.reverse[G1]((l  * g) :/ r / r)
      case Div10(l, r) => l.reverse[G0](g  / r) ++ r.reverse[G1]((l :* g) :/ r / r)
      case Div11(l, r) => l.reverse[G1](g :/ r) ++ r.reverse[G1]((l :* g)  / r / r)

      // Math
      case Sin1(v) => v.reverse[G1](g :* Cos1(v))
      case Cos1(v) => v.reverse[G1](-g :* Sin1(v))
      case Tan1(v) => v.reverse[G1](g :* (One1(v) + (Tan1(v) * Tan1(v))))

      case Asin1(v) => v.reverse[G1](g :*  (One1(v) / Sqrt1(One1(v) - (v * v))))
      case Acos1(v) => v.reverse[G1](g :* -(One1(v) / Sqrt1(One1(v) - (v * v))))
      case Atan1(v) => v.reverse[G1](g :* -(One1(v) / (One1(v) + (v * v))))

      case Sinh1(v) => v.reverse[G1](g :* Cosh1(v))
      case Cosh1(v) => v.reverse[G1](g :* Sinh1(v))
      case Tanh1(v) => v.reverse[G1](g :* (One1(v) - (Tanh1(v) * Tanh1(v))))

      case Ln1(v)      => v.reverse[G1](g :/ v)
      case Exp1(v)     => v.reverse[G1](g :* Exp1(v))
      case Sqrt1(v)    => v.reverse[G1](g :* (Half1(v) / Sqrt1(v)))
      case Pow01(l, r) => {
        val lhs = l.reverse[G1](g :* r * Pow01(l, r :- One0()))
        val rhs = r.reverse[G1](g * Ln0(l) :* Pow01(l, r))
        lhs ++ rhs
      }
      case Pow10(l, r) => {
        val lhs = l.reverse[G1](g  * r :* Pow10(l, r  - One0()))
        val rhs = r.reverse[G1](g :* Ln1(l) * Pow10(l, r))
        lhs ++ rhs
      }
      case Pow11(l, r) => {
        val lhs = l.reverse[G1](g :* r * Pow11(l, r :- One0()))
        val rhs = r.reverse[G1](g :* Ln1(l) * Pow11(l, r))
        lhs ++ rhs
      }

      // Experimental
      case Abs1(v)     => Where1_1(Gt10(v, Zero0()), v, -v).reverse(g)
      case Max11(l, r) => Where1_1(Gt11(l, r), l, r).reverse(g)
      case Min11(l, r) => Where1_1(Lt11(l, r), l, r).reverse(g)
    }
  }

  implicit def reverse11: Reverse[N1, N1] = new Reverse[N1, N1] {

    private[this] type N = N1
    private[this] type G1 = N1

    def reverse(n: N, g: G1): Grad = n match {
      // Leaf nodes
      case Var1(_, shape)   => Grad(n, g :* One0())
      case Zero1(shape)     => Grad.empty
      case Half1(shape)     => Grad.empty
      case One1(shape)      => Grad.empty
      case Const1(_, shape) => Grad.empty

      // Unary ops
      case Pos1(v) => v.reverse[G1](+g)
      case Neg1(v) => v.reverse[G1](-g)
      case Transpose1(v) => v.reverse[G1](g.T)

      // Binary ops
      case Add01(l, r) => l.reverse[G1](g) ++ r.reverse[G1] (g)
      case Add10(l, r) => l.reverse[G1](g) ++ r.reverse[G1](g)
      case Add11(l, r) => l.reverse[G1](g) ++ r.reverse[G1] (g)

      case Sub01(l, r) => l.reverse[G1](g) ++ r.reverse[G1] (-g)
      case Sub10(l, r) => l.reverse[G1](g) ++ r.reverse[G1](-g)
      case Sub11(l, r) => l.reverse[G1](g) ++ r.reverse[G1] (-g)

      case Mul01(l, r) => l.reverse[G1](g * r)  ++ r.reverse[G1](l :* g)
      case Mul10(l, r) => l.reverse[G1](g :* r) ++ r.reverse[G1](l * g)
      case Mul11(l, r) => l.reverse[G1](g * r)  ++ r.reverse[G1](l * g)

      case Div01(l, r) => l.reverse[G1](g / r)  ++ r.reverse[G1](l :* g  / r  / r)
      case Div10(l, r) => l.reverse[G1](g :/ r) ++ r.reverse[G1](l  * g :/ r :/ r)
      case Div11(l, r) => l.reverse[G1](g / r)  ++ r.reverse[G1](l  * g  / r  / r)

      // Math
      case Sin1(v) => v.reverse[G1](g * Cos1(v))
      case Cos1(v) => v.reverse[G1](-g * Sin1(v))
      case Tan1(v) => v.reverse[G1](g * (One1(v) + (Tan1(v) * Tan1(v))))

      case Asin1(v) => v.reverse[G1](g *  (One1(v) / Sqrt1(One1(v) - (v * v))))
      case Acos1(v) => v.reverse[G1](g * -(One1(v) / Sqrt1(One1(v) - (v * v))))
      case Atan1(v) => v.reverse[G1](g * -(One1(v) / (One1(v) + (v * v))))

      case Sinh1(v) => v.reverse[G1](g * Cosh1(v))
      case Cosh1(v) => v.reverse[G1](g * Sinh1(v))
      case Tanh1(v) => v.reverse[G1](g * (One1(v) - (Tanh1(v) * Tanh1(v))))

      case Ln1(v)      => v.reverse[G1](g / v)
      case Exp1(v)     => v.reverse[G1](g * Exp1(v))
      case Sqrt1(v)    => v.reverse[G1](g * (Half1(v) / Sqrt1(v)))
      case Pow01(l, r) => {
        val lhs = l.reverse[G1]((g  * r) * Pow01(l, r :- One0()))
        val rhs = r.reverse[G1]((g :* Ln0(l)) * Pow01(l, r))
        lhs ++ rhs
      }
      case Pow10(l, r) => {
        val lhs = l.reverse[G1]((g :* r) * Pow10(l, r  - One0()))
        val rhs = r.reverse[G1](g * Ln1(l) * Pow10(l, r))
        lhs ++ rhs
      }
      case Pow11(l, r) => {
        val lhs = l.reverse[G1](g * r * Pow11(l, r :- One0()))
        val rhs = r.reverse[G1](g * Ln1(l) * Pow11(l, r))
        lhs ++ rhs
      }

      // Experimental
      case Abs1(v)     => Where1_1(Gt10(v, Zero0()), v, -v).reverse(g)
      case Max11(l, r) => Where1_1(Gt11(l, r), l, r).reverse(g)
      case Min11(l, r) => Where1_1(Lt11(l, r), l, r).reverse(g)
    }
  }

  implicit def reverse20: Reverse[N2, N0] = new Reverse[N2, N0] {

    private[this] type N = N2
    private[this] type G0 = N0
    private[this] type G2 = N2

    def reverse(n: N, g: G0): Grad = n match {
      // Leaf nodes
      case Var2(_, shape)   => Grad(n, g * One0())
      case Zero2(shape)     => Grad.empty
      case Half2(shape)     => Grad.empty
      case One2(shape)      => Grad.empty
      case Const2(_, shape) => Grad.empty

      // Unary ops
      case Pos2(v) => v.reverse[G0](+g)
      case Neg2(v) => v.reverse[G0](-g)
      case Transpose2(v) => v.reverse[G0](g)

      // Binary ops
      case Add02(l, r) => l.reverse[G0](g) ++ r.reverse[G0](g)
      case Add20(l, r) => l.reverse[G0](g) ++ r.reverse[G0](g)
      case Add22(l, r) => l.reverse[G0](g) ++ r.reverse[G0](g)

      case Sub02(l, r) => l.reverse[G0](g) ++ r.reverse[G0](-g)
      case Sub20(l, r) => l.reverse[G0](g) ++ r.reverse[G0](-g)
      case Sub22(l, r) => l.reverse[G0](g) ++ r.reverse[G0](-g)

      case Mul02(l, r) => l.reverse[G2](g :* r) ++ r.reverse[G0](l  * g)
      case Mul20(l, r) => l.reverse[G0](g  * r) ++ r.reverse[G2](l :* g)
      case Mul22(l, r) => l.reverse[G2](g :* r) ++ r.reverse[G2](l :* g)

      case Div02(l, r) => l.reverse[G2](g :/ r) ++ r.reverse[G2]((l * g)  :/ r  / r)
      case Div20(l, r) => l.reverse[G0](g  / r) ++ r.reverse[G2]((l :* g) :/ r :/ r)
      case Div22(l, r) => l.reverse[G2](g :/ r) ++ r.reverse[G2]((l :* g)  / r  / r)

      // Math
      case Sin2(v) => v.reverse[G2](g :* Cos2(v))
      case Cos2(v) => v.reverse[G2](-g :* Sin2(v))
      case Tan2(v) => v.reverse[G2](g :* (One2(v.shape) + (Tan2(v) * Tan2(v))))

      case Asin2(v) => v.reverse[G2](g :*  (One2(v.shape) / Sqrt2(One2(v.shape) - (v * v))))
      case Acos2(v) => v.reverse[G2](g :* -(One2(v.shape) / Sqrt2(One2(v.shape) - (v * v))))
      case Atan2(v) => v.reverse[G2](g :* -(One2(v.shape) / (One2(v.shape) + (v * v))))

      case Sinh2(v) => v.reverse[G2](g :* Cosh2(v))
      case Cosh2(v) => v.reverse[G2](g :* Sinh2(v))
      case Tanh2(v) => v.reverse[G2](g :* (One2(v.shape) - (Tanh2(v) * Tanh2(v))))

      case Ln2(v)      => v.reverse[G2](g :/ v)
      case Exp2(v)     => v.reverse[G2](g :* Exp2(v))
      case Sqrt2(v)    => v.reverse[G2](g :* (Half2(v.shape) / Sqrt2(v)))
      case Pow02(l, r) => {
        val lhs = l.reverse[G2]((g :* r) * Pow02(l, r :- One0()))
        val rhs = r.reverse[G2]((g * Ln0(l)) :* Pow02(l, r))
        lhs ++ rhs
      }
      case Pow20(l, r) => {
        val lhs = l.reverse[G2]((g * r) :* Pow20(l, r  - One0()))
        val rhs = r.reverse[G2](g :* Ln2(l) * Pow20(l, r))
        lhs ++ rhs
      }
      case Pow22(l, r) => {
        val lhs = l.reverse[G2](g :* r * Pow22(l, r :- One0()))
        val rhs = r.reverse[G2](g :* Ln2(l) * Pow22(l, r))
        lhs ++ rhs
      }

      // Experimental
      case Abs2(v)     => Where2_2(Gt20(v, Zero0()), v, -v).reverse(g)
      case Max22(l, r) => Where2_2(Gt22(l, r), l, r).reverse(g)
      case Min22(l, r) => Where2_2(Lt22(l, r), l, r).reverse(g)

    }
  }

  implicit def reverse22: Reverse[N2, N2] = new Reverse[N2, N2] {

    private[this] type N = N2
    private[this] type G = N2

    def reverse(n: N, g: G): Grad = n match {
      // Leaf nodes
      case Var2(_, shape)   => Grad(n, g :* One0())
      case Zero2(shape)     => Grad.empty
      case Half2(shape)     => Grad.empty
      case One2(shape)      => Grad.empty
      case Const2(_, shape) => Grad.empty

      // Unary ops
      case Pos2(v) => v.reverse[G](+g)
      case Neg2(v) => v.reverse[G](-g)
      case Transpose2(v) => v.reverse[G](g.T)

      // Binary ops
      case Add02(l, r) => l.reverse[G](g) ++ r.reverse[G](g)
      case Add20(l, r) => l.reverse[G](g) ++ r.reverse[G](g)
      case Add22(l, r) => l.reverse[G](g) ++ r.reverse[G](g)

      case Sub02(l, r) => l.reverse[G](g) ++ r.reverse[G](-g)
      case Sub20(l, r) => l.reverse[G](g) ++ r.reverse[G](-g)
      case Sub22(l, r) => l.reverse[G](g) ++ r.reverse[G](-g)

      case Mul02(l, r) => l.reverse[G](g  * r) ++ r.reverse[G](l :* g)
      case Mul20(l, r) => l.reverse[G](g :* r) ++ r.reverse[G](l  * g)
      case Mul22(l, r) => l.reverse[G](g  * r) ++ r.reverse[G](l  * g)

      case Div02(l, r) => l.reverse[G](g  / r) ++ r.reverse[G]((l :* g)  / r / r)
      case Div20(l, r) => l.reverse[G](g :/ r) ++ r.reverse[G]((l  * g) :/ r / r)
      case Div22(l, r) => l.reverse[G](g  / r) ++ r.reverse[G]((l  * g)  / r / r)

      // Math
      case Sin2(v) => v.reverse[G](g * Cos2(v))
      case Cos2(v) => v.reverse[G](-g * Sin2(v))
      case Tan2(v) => v.reverse[G](g * (One2(v.shape) + (Tan2(v) * Tan2(v))))

      case Asin2(v) => v.reverse[G](g *  (One2(v.shape) / Sqrt2(One2(v.shape) - (v * v))))
      case Acos2(v) => v.reverse[G](g * -(One2(v.shape) / Sqrt2(One2(v.shape) - (v * v))))
      case Atan2(v) => v.reverse[G](g * -(One2(v.shape) / (One2(v.shape) + (v * v))))

      case Sinh2(v) => v.reverse[G](g * Cosh2(v))
      case Cosh2(v) => v.reverse[G](g * Sinh2(v))
      case Tanh2(v) => v.reverse[G](g * (One2(v.shape) - (Tanh2(v) * Tanh2(v))))

      case Ln2(v)      => v.reverse[G](g / v)
      case Exp2(v)     => v.reverse[G](g * Exp2(v))
      case Sqrt2(v)    => v.reverse[G](g * (Half2(v.shape) / Sqrt2(v)))
      case Pow02(l, r) => {
        val lhs = l.reverse[G]((g * r) * Pow02(l, r :- One0()))
        val rhs = r.reverse[G]((g :* Ln0(l)) * Pow02(l, r))
        lhs ++ rhs
      }
      case Pow20(l, r) => {
        val lhs = l.reverse[G]((g :* r) * Pow20(l, r  - One0()))
        val rhs = r.reverse[G](g * Ln2(l) * Pow20(l, r))
        lhs ++ rhs
      }
      case Pow22(l, r) => {
        val lhs = l.reverse[G](g * r * Pow22(l, r :- One0()))
        val rhs = r.reverse[G](g * Ln2(l) * Pow22(l, r))
        lhs ++ rhs
      }

      // Experimental
      case Abs2(v)     => Where2_2(Gt20(v, Zero0()), v, -v).reverse(g)
      case Max22(l, r) => Where2_2(Gt22(l, r), l, r).reverse(g)
      case Min22(l, r) => Where2_2(Lt22(l, r), l, r).reverse(g)

      //case Matmul12(l, r) => l.reverse[G](Matmul22(g, r)) ++ r.reverse[G](Matmul12(l, g))
      //case Matmul21(l, r) => l.reverse[G](Matmul21(g, r)) ++ r.reverse[G](Matmul22(l, g))
      //case Matmul22(l, r) => l.reverse[G](Matmul22(g, r)) ++ r.reverse[G](Matmul22(l, g))
    }
  }

}

