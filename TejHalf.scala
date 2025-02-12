// import scala.math._
// import scala.reflect._
// import spire.algebra._
// import spire.std.ArraySupport
// import spire.syntax.isReal._
// import spire.syntax.nroot._
// import spire.syntax.vectorSpace._
// import scala.specialized as sp
// import vecxt.all.*
// import vecxt.BoundsCheck.DoBoundsCheck.no

// /** Used to implicitly define the dimensionality of the Tej space.
//   * @param dimension
//   *   the number of dimensions.
//   */

// inline def anyIsZero(n: Any): Boolean =
//   n match {
//     case x if x == 0                => true
//     case c: ScalaNumericConversions => c.isValidInt && c.toInt == 0
//     case _                          => false
//   }

// inline def anyToDouble(n: Any): Double =
//   n match {
//     case n: Byte                    => n.toDouble
//     case n: Short                   => n.toDouble
//     case n: Char                    => n.toDouble
//     case n: Int                     => n.toDouble
//     case n: Long                    => n.toDouble
//     case n: Float                   => n.toDouble
//     case n: Double                  => n
//     case c: ScalaNumericConversions => c.toDouble
//     case _ =>
//       throw new UnsupportedOperationException(s"$n is not a ScalaNumber")
//   }

// inline def anyToLong(n: Any): Long =
//   n match {
//     case n: Byte                    => n.toLong
//     case n: Short                   => n.toLong
//     case n: Char                    => n.toLong
//     case n: Int                     => n.toLong
//     case n: Long                    => n
//     case n: Float                   => n.toLong
//     case n: Double                  => n.toLong
//     case c: ScalaNumericConversions => c.toLong
//     case _ =>
//       throw new UnsupportedOperationException(s"$n is not a ScalaNumber")
//   }

// inline def anyIsWhole(n: Any): Boolean =
//   n match {
//     case _: Byte                    => true
//     case _: Short                   => true
//     case _: Char                    => true
//     case _: Int                     => true
//     case _: Long                    => true
//     case n: Float                   => n.isWhole
//     case n: Double                  => n.isWhole
//     case c: ScalaNumericConversions => c.isWhole
//     case _ =>
//       throw new UnsupportedOperationException(s"$n is not a ScalaNumber")
//   }

// inline def anyIsValidInt(n: Any): Boolean =
//   n match {
//     case _: Byte                    => true
//     case _: Short                   => true
//     case _: Char                    => true
//     case _: Int                     => true
//     case n: Long                    => n.isValidInt
//     case n: Float                   => n.isValidInt
//     case n: Double                  => n.isValidInt
//     case c: ScalaNumericConversions => c.isValidInt
//     case _ =>
//       throw new UnsupportedOperationException(s"$n is not a ScalaNumber")
//   }

// // scalastyle:off regex
// /** ==Overview==
//   * A simple implementation of N-dimensional dual numbers, for automatically
//   * computing exact derivatives of functions. This code (and documentation)
//   * closely follow the one in Google's "Ceres" library of non-linear
//   * least-squares solvers (see <a
//   * href="http://code.google.com/p/ceres-solver">Sameer Agarwal, Keir Mierle,
//   * and others: Ceres Solver.</a>)
//   *
//   * While a complete treatment of the mechanics of automatic differentiation is
//   * beyond the scope of this header (see
//   * http://en.wikipedia.org/wiki/Automatic_differentiation for details), the
//   * basic idea is to extend normal arithmetic with an extra element "h" such
//   * that h != 0, but h^2^ = 0. Dual numbers are extensions of the real numbers
//   * analogous to complex numbers: whereas complex numbers augment the reals by
//   * introducing an imaginary unit i such that i^2^ = -1, dual numbers introduce
//   * an "infinitesimal" unit h such that h^2^ = 0. Analogously to a complex
//   * number c = x + y*i, a dual number d = x * y*h has two components: the "real"
//   * component x, and an "infinitesimal" component y. Surprisingly, this leads to
//   * a convenient method for computing exact derivatives without needing to
//   * manipulate complicated symbolic expressions.
//   *
//   * For example, consider the function
//   * {{{
//   *   f(x) = x * x ,
//   * }}}
//   * evaluated at 10. Using normal arithmetic,
//   * {{{
//   * f(10) = 100, and df/dx(10) = 20.
//   * }}}
//   * Next, augment 10 with an infinitesimal h to get:
//   * {{{
//   *   f(10 + h) = (10 + h) * (10 + h)
//   *             = 100 + 2 * 10 * h + h * h
//   *             = 100 + 20 * h       +---
//   *                     +-----       |
//   *                     |            +--- This is zero
//   *                     |
//   *                     +----------------- This is df/dx
//   * }}}
//   * Note that the derivative of f with respect to x is simply the infinitesimal
//   * component of the value of f(x + h). So, in order to take the derivative of
//   * any function, it is only necessary to replace the numeric "object" used in
//   * the function with one extended with infinitesimals. The class Tej, defined
//   * in this header, is one such example of this, where substitution is done with
//   * generics.
//   *
//   * To handle derivatives of functions taking multiple arguments, different
//   * infinitesimals are used, one for each variable to take the derivative of.
//   * For example, consider a scalar function of two scalar parameters x and y:
//   * {{{
//   *   f(x, y) = x * x + x * y
//   * }}}
//   * Following the technique above, to compute the derivatives df/dx and df/dy
//   * for f(1, 3) involves doing two evaluations of f, the first time replacing x
//   * with x + h, the second time replacing y with y + h.
//   *
//   * For df/dx:
//   * {{{
//   *   f(1 + h, y) = (1 + h) * (1 + h) + (1 + h) * 3
//   *               = 1 + 2 * h + 3 + 3 * h
//   *               = 4 + 5 * h
//   *
//   *   Therefore df/dx = 5
//   * }}}
//   * For df/dy:
//   * {{{
//   *   f(1, 3 + h) = 1 * 1 + 1 * (3 + h)
//   *               = 1 + 3 + h
//   *               = 4 + h
//   *
//   *   Therefore df/dy = 1
//   * }}}
//   * To take the gradient of f with the implementation of dual numbers ("Tejs")
//   * in this file, it is necessary to create a single Tej type which has
//   * components for the derivative in x and y, and pass them to a routine
//   * computing function f. It is convenient to use a generic version of f, that
//   * can be called also with non-Tej numbers for standard evaluation:
//   * {{{
//   *   def f[@specialized(Double) T : Field](x: T, y: T): T = x * x + x * y
//   *
//   *   val xValue = 9.47892774
//   *   val yValue = 0.287740
//   *
//   *   // The "2" means there should be 2 dual number components.
//   *   implicit val dimension = TejDim(2)
//   *   val x: Tej[Double] = xValue + Tej.h[Double](0);  // Pick the 0th dual number for x.
//   *   val y: Tej[Double] = yValue + Tej.h[Double](1);  // Pick the 1th dual number for y.
//   *
//   *   val z: Tej[Double] = f(x, y);
//   *   println("df/dx = " + z.infinitesimal(0) + ", df/dy = " + z.infinitesimal(1));
//   * }}}
//   *
//   * For the more mathematically inclined, this file implements first-order
//   * "Tejs". A 1st order Tej is an element of the ring
//   * {{{
//   *   T[N] = T[t_1, ..., t_N] / (t_1, ..., t_N)^2
//   * }}}
//   * which essentially means that each Tej consists of a "scalar" value 'a' from
//   * T and a 1st order perturbation vector 'v' of length N:
//   * {{{
//   *   x = a + \sum_i v[i] t_i
//   * }}}
//   * A shorthand is to write an element as x = a + u, where u is the
//   * perturbation. Then, the main point about the arithmetic of Tejs is that the
//   * product of perturbations is zero:
//   * {{{
//   *   (a + u) * (b + v) = ab + av + bu + uv
//   *                     = ab + (av + bu) + 0
//   * }}}
//   * which is what operator* implements below. Addition is simpler:
//   * {{{
//   *   (a + u) + (b + v) = (a + b) + (u + v).
//   * }}}
//   * The only remaining question is how to evaluate the function of a Tej, for
//   * which we use the chain rule:
//   * {{{
//   *   f(a + u) = f(a) + f'(a) u
//   * }}}
//   * where f'(a) is the (scalar) derivative of f at a.
//   *
//   * By pushing these things through generics, we can write routines that at same
//   * time evaluate mathematical functions and compute their derivatives through
//   * automatic differentiation.
//   */

// extension (t: Tej[Array[Double]])
//   def show = {
//     val real = t.real.mkString("(", ", ", ")")
//     val infinitesimal = t.infinitesimal.printMat

//     println(s"Tej($real, ${infinitesimal})")
//   }

// case class TejDim(dimension: Int) {
//   require(dimension > 0)
// }

// object Tej extends TejInstance {
//   // No-arg c.tor makes a zero Tej
//   def apply[@sp(Float, Double) T](implicit
//       c: ClassTag[T],
//       d: TejDim,
//       s: Semiring[T]
//   ): Tej[T] = Tej(s.zero)

//   // From real.
//   def apply[@sp(Float, Double) T](
//       real: T
//   )(implicit c: ClassTag[T], d: TejDim, s: Semiring[T]): Tej[T] =
//     new Tej(real, Matrix.eye[T](d.dimension))

//   // // From real, to compute k-th partial derivative.
//   // def apply[@sp(Float, Double) T](a: T, k: Int)(implicit
//   //     c: ClassTag[T],
//   //     d: TejDim,
//   //     r: Rig[T]
//   // ): Tej[T] = {
//   //   val v = Array.fill[T](d.dimension)(r.zero)
//   //   v(k) = r.one
//   //   new Tej(a, v)
//   // }

//   def one[@sp(Float, Double) T](implicit
//       c: ClassTag[T],
//       d: TejDim,
//       r: Rig[T]
//   ): Tej[T] = Tej(r.one)

//   def zero[@sp(Float, Double) T](implicit
//       c: ClassTag[T],
//       d: TejDim,
//       s: Semiring[T]
//   ): Tej[T] = Tej(s.zero)

//   def fromInt[@sp(Float, Double) T](
//       n: Int
//   )(implicit c: ClassTag[T], d: TejDim, r: Ring[T]): Tej[T] =
//     Tej(r.fromInt(n))

//   implicit def intToTej(n: Int)(implicit d: TejDim): Tej[Double] = {
//     doubleToTej(n.toDouble)
//   }

//   implicit def longToTej(n: Long)(implicit d: TejDim): Tej[Double] = {
//     doubleToTej(n.toDouble)
//   }

//   implicit def floatToTej(n: Float)(implicit d: TejDim): Tej[Float] = {
//     new Tej(n.toFloat, Matrix.eye[Float](d.dimension))
//   }

//   implicit def doubleToTej(n: Double)(implicit d: TejDim): Tej[Double] = {
//     new Tej(n, Matrix.eye[Double](d.dimension))
//   }

//   implicit def bigIntToTej(n: BigInt)(implicit d: TejDim): Tej[BigDecimal] = {
//     bigDecimalToTej(BigDecimal(n))
//   }

//   implicit def bigDecimalToTej(
//       n: BigDecimal
//   )(implicit d: TejDim): Tej[BigDecimal] = {
//     new Tej(n, Matrix.eye[BigDecimal](d.dimension))
//   }
// }

// @SerialVersionUID(0L)
// final case class Tej[@sp(Float, Double) T](real: T, infinitesimal: Matrix[T])
//     extends ScalaNumber
//     with ScalaNumericConversions
//     with Serializable { lhs =>

//   import spire.syntax.order._

//   def dimension: Int = infinitesimal.rows.toInt
//   def matDim = (dimension, dimension)
//   implicit def TejDimension: TejDim = TejDim(dimension)

//   /** This is consistent with abs
//     */
//   def signum(implicit r: Signed[T]): Int = real.signum

//   def asTuple: (T, Matrix[T]) = (real, infinitesimal)

//   def isReal: Boolean = infinitesimal.raw.forall(anyIsZero)
//   def isZero: Boolean = anyIsZero(real) && isReal
//   def isInfinitesimal: Boolean = anyIsZero(real) && !isReal

//   def eqv(b: Tej[T])(implicit o: Eq[T]): Boolean = {
//     real === b.real && ArraySupport.eqv(infinitesimal.raw, b.infinitesimal.raw)
//   }
//   def neqv(b: Tej[T])(implicit o: Eq[T]): Boolean = {
//     !this.eqv(b)
//   }

//   def unary_-(implicit
//       f: Field[T],
//       ct: ClassTag[T],
//       v: VectorSpace[Array[T], T]
//   ): Tej[T] = {
//     new Tej(
//       -real,
//       Matrix(infinitesimal.raw.map(-_), (dimension, dimension))
//     )
//   }

//   def +(b: T)(implicit f: Field[T]): Tej[T] = new Tej(real + b, infinitesimal)
//   def -(b: T)(implicit f: Field[T]): Tej[T] = new Tej(real - b, infinitesimal)
//   def *(b: T)(implicit
//       f: Field[T],
//       ct: ClassTag[T],
//       v: VectorSpace[Array[T], T]
//   ): Tej[T] = {
//     new Tej(real * b, Matrix(infinitesimal.raw.map(_ * b), matDim))
//   }
//   def /(b: T)(implicit
//       f: Field[T],
//       ct: ClassTag[T],
//       v: VectorSpace[Array[T], T]
//   ): Tej[T] = {
//     new Tej(real / b, Matrix(infinitesimal.raw.map(_ / b), matDim))
//   }
//   def +(
//       b: Tej[T]
//   )(implicit f: Field[T], v: VectorSpace[Array[T], T]): Tej[T] = {
//     new Tej(
//       real + b.real,
//       Matrix(infinitesimal.raw + b.infinitesimal.raw, matDim)
//     )
//   }
//   def -(
//       b: Tej[T]
//   )(implicit f: Field[T], v: VectorSpace[Array[T], T]): Tej[T] = {
//     new Tej(
//       real - b.real,
//       Matrix(infinitesimal.raw - b.infinitesimal.raw, matDim)
//     )
//   }
//   // Multiplication rule for differentials:
//   //
//   //    (a + du)(b + dv) ~= ab + a dv + b du
//   //
//   // because du dv ~= 0
//   def *(
//       b: Tej[T]
//   )(implicit f: Field[T], v: VectorSpace[Array[T], T]): Tej[T] = {
//     new Tej(real * b.real, b.real *: infinitesimal + real *: b.infinitesimal)
//   }

//   def /(
//       b: Tej[T]
//   )(implicit f: Field[T], v: VectorSpace[Array[T], T]): Tej[T] = {
//     // Division rule for differentials:
//     //
//     //   a + du   (a + du)(b - dv)    ab - a dv + b du   a    a       1      a   1         a
//     //   ------ = ---------------- ~= ---------------- = - - --- dv + - du = - + - * (du - - dv)
//     //   b + dv   (b + dv)(b - dv)           b^2         b   b^2      b      b   b         b
//     //
//     // which holds because dv dv = du dv = 0.
//     val br_inv: T = f.one / b.real
//     val ar_div_br: T = real * br_inv
//     new Tej(
//       ar_div_br,
//       br_inv *: (infinitesimal - (ar_div_br *: b.infinitesimal))
//     )
//   }

//   def /~(b: Tej[T])(implicit
//       c: ClassTag[T],
//       f: Field[T],
//       r: IsReal[T],
//       v: VectorSpace[Array[T], T]
//   ): Tej[T] = {
//     val q = this / b
//     new Tej[T](q.real.floor, Matrix(q.infinitesimal.raw.map(r.floor), matDim))
//   }

//   def %(b: Tej[T])(implicit
//       c: ClassTag[T],
//       f: Field[T],
//       r: IsReal[T],
//       v: VectorSpace[Array[T], T]
//   ): Tej[T] = {
//     this - ((this /~ b) * b)
//   }

//   def /%(
//       b: Tej[T]
//   )(implicit
//       c: ClassTag[T],
//       f: Field[T],
//       r: IsReal[T],
//       v: VectorSpace[Array[T], T]
//   ): (Tej[T], Tej[T]) = {
//     val q = this /~ b
//     (q, this - (q * b))
//   }

//   def **(b: Int)(implicit f: Field[T], v: VectorSpace[Array[T], T]): Tej[T] =
//     pow(b)

//   def nroot(
//       k: Int
//   )(implicit
//       f: Field[T],
//       o: Order[T],
//       s: Signed[T],
//       t: Trig[T],
//       v: VectorSpace[Array[T], T]
//   ): Tej[T] = {
//     pow(f.fromInt(k).reciprocal)
//   }

//   def **(
//       b: Tej[T]
//   )(implicit
//       c: ClassTag[T],
//       f: Field[T],
//       o: Order[T],
//       s: Signed[T],
//       t: Trig[T],
//       v: VectorSpace[Array[T], T]
//   ): Tej[T] = {
//     pow(b)
//   }

//   def floor(implicit c: ClassTag[T], r: IsReal[T]): Tej[T] = {
//     new Tej(real.floor, Matrix(infinitesimal.raw.map(r.floor), matDim))
//   }

//   def ceil(implicit c: ClassTag[T], r: IsReal[T]): Tej[T] = {
//     new Tej(real.ceil, Matrix(infinitesimal.raw.map(r.ceil), matDim))
//   }

//   def round(implicit c: ClassTag[T], r: IsReal[T]): Tej[T] = {
//     new Tej(real.round, Matrix(infinitesimal.raw.map(r.round), matDim))
//   }

//   // Elementary math functions
//   // In general, f(a + du) ~= f(a) + f'(a) du .

//   /** abs(x + du) ~= x + du or -(x + du)
//     */
//   def abs(implicit
//       f: Field[T],
//       o: Order[T],
//       s: Signed[T],
//       v: VectorSpace[Array[T], T]
//   ): Tej[T] = {
//     if (real < f.zero) new Tej(-real, -infinitesimal)
//     else this
//   }

//   // spire.math. does not define this pow generically, so there it is
//   private def powScalarToScalar(
//       b: T,
//       e: T
//   )(implicit f: Field[T], o: Order[T], s: Signed[T], t: Trig[T]): T = {
//     if (e === f.zero) {
//       f.one
//     } else if (b === f.zero) {
//       if (e < f.zero) throw new Exception("raising 0 to a negative power")
//       else f.zero
//     } else {
//       spire.math.exp(e * spire.math.log(b))
//     }
//   }

//   // pow -- base is a constant, exponent (this) is a differentiable function.
//   // b^(p + du) ~= b^p + b^p * log(b) du
//   def powScalarToTej(
//       a: T
//   )(implicit
//       c: ClassTag[T],
//       f: Field[T],
//       m: CModule[Array[T], T],
//       o: Order[T],
//       s: Signed[T],
//       t: Trig[T]
//   ): Tej[T] = {
//     if (isZero) {
//       Tej.one[T]
//     } else {
//       val tmp = powScalarToScalar(a, real)
//       new Tej(tmp, (spire.math.log(a) * tmp) *: infinitesimal)
//     }
//   }

//   /** pow -- base (this) is a differentiable function, exponent is a constant.
//     * {{{
//     * pow(a + du, p) ~= pow(a, p) + p * pow(a, p-1) du
//     * }}}
//     */
//   def pow(p: T)(implicit
//       f: Field[T],
//       o: Order[T],
//       s: Signed[T],
//       t: Trig[T],
//       v: VectorSpace[Array[T], T]
//   ): Tej[T] = {
//     val tmp: T = p * powScalarToScalar(real, p - f.one)
//     new Tej(powScalarToScalar(real, p), tmp *: infinitesimal)
//   }

//   // As above, integer exponent.
//   def pow(p: Int)(implicit f: Field[T], v: VectorSpace[Array[T], T]): Tej[T] = {
//     val tmp = p * f.pow(real, p - 1)
//     new Tej(f.pow(real, p), tmp *: infinitesimal)
//   }

//   /** pow -- both base (this) and exponent are differentiable functions.
//     * {{{
//     * (a + du)^(b + dv) ~= a^b + b * a^(b-1) du + a^b log(a) dv
//     * }}}
//     */
//   def pow(
//       b: Tej[T]
//   )(implicit
//       c: ClassTag[T],
//       f: Field[T],
//       m: CModule[Array[T], T],
//       o: Order[T],
//       s: Signed[T],
//       t: Trig[T]
//   ): Tej[T] = {
//     if (b.isZero) {
//       Tej.one[T]
//     } else {
//       val tmp1 = powScalarToScalar(real, b.real)
//       val tmp2 = b.real * powScalarToScalar(real, b.real - f.one)
//       val tmp3 = tmp1 * spire.math.log(real)
//       new Tej(tmp1, (tmp2 *: infinitesimal) + (tmp3 *: b.infinitesimal))
//     }
//   }

//   /** log(a + du) ~= log(a) + du / a
//     */
//   def log(implicit
//       f: Field[T],
//       t: Trig[T],
//       v: VectorSpace[Array[T], T]
//   ): Tej[T] = {
//     new Tej(spire.math.log(real), (f.one / real) *: infinitesimal)
//   }

//   /** sqrt(a + du) ~= sqrt(a) + du / (2 sqrt(a))
//     */
//   def sqrt(implicit
//       f: Field[T],
//       n: NRoot[T],
//       v: VectorSpace[Array[T], T]
//   ): Tej[T] = {
//     val sa = real.sqrt
//     val oneHalf = f.one / (f.one + f.one)
//     new Tej(sa, (oneHalf / sa) *: infinitesimal)
//   }

//   /** acos(a + du) ~= acos(a) - 1 / sqrt(1 - a**2) du
//     */
//   def acos(implicit
//       f: Field[T],
//       n: NRoot[T],
//       t: Trig[T],
//       v: VectorSpace[Array[T], T]
//   ): Tej[T] = {
//     val tmp = -f.one / spire.math.sqrt(f.one - real * real)
//     new Tej(spire.math.acos(real), tmp *: infinitesimal)
//   }

//   /** asin(a + du) ~= asin(a) - 1 / sqrt(1 - a**2) du
//     */
//   def asin(implicit
//       f: Field[T],
//       n: NRoot[T],
//       t: Trig[T],
//       v: VectorSpace[Array[T], T]
//   ): Tej[T] = {
//     val tmp = f.one / spire.math.sqrt(f.one - real * real)
//     new Tej(spire.math.asin(real), tmp *: infinitesimal)
//   }

//   /** atan(a + du) ~= atan(a) + 1 / (1 + a**2) du
//     */
//   def atan(implicit
//       f: Field[T],
//       t: Trig[T],
//       v: VectorSpace[Array[T], T]
//   ): Tej[T] = {
//     val tmp = f.one / (f.one + real * real)
//     new Tej(spire.math.atan(real), tmp *: infinitesimal)
//   }

//   /** Defined with "this" as the y coordinate:
//     * {{{
//     * this.atan2(a) == atan2(this, a) == atan(this / a) atan2(b + dv, a + du) ~= atan2(b, a) + (- b du + a dv) / (a^2 + b^2)
//     * }}}
//     */
//   def atan2(
//       a: Tej[T]
//   )(implicit f: Field[T], t: Trig[T], v: VectorSpace[Array[T], T]): Tej[T] = {
//     val tmp = f.one / (a.real * a.real + real * real)
//     new Tej(
//       spire.math.atan2(real, a.real),
//       ((tmp * (-real)) *: a.infinitesimal) + ((tmp * a.real) *: infinitesimal)
//     )
//   }

//   /** exp(a + du) ~= exp(a) + exp(a) du
//     */
//   def exp(implicit t: Trig[T], v: VectorSpace[Array[T], T]): Tej[T] = {
//     val ea = spire.math.exp(real)
//     new Tej[T](ea, ea *: infinitesimal)
//   }

//   /** sin(a + du) ~= sin(a) + cos(a) du
//     */
//   def sin(implicit t: Trig[T], v: VectorSpace[Array[T], T]): Tej[T] = {
//     new Tej(spire.math.sin(real), spire.math.cos(real) *: infinitesimal)
//   }

//   /** sinh(a + du) ~= sinh(a) + cosh(a) du
//     */
//   def sinh(implicit t: Trig[T], v: VectorSpace[Array[T], T]): Tej[T] = {
//     new Tej(spire.math.sinh(real), spire.math.cosh(real) *: infinitesimal)
//   }

//   /** cos(a + du) ~= cos(a) - sin(a) du
//     */
//   def cos(implicit
//       f: Field[T],
//       t: Trig[T],
//       v: VectorSpace[Array[T], T]
//   ): Tej[T] = {
//     new Tej(spire.math.cos(real), -spire.math.sin(real) *: infinitesimal)
//   }

//   /** cosh(a + du) ~= cosh(a) + sinh(a) du
//     */
//   def cosh(implicit t: Trig[T], v: VectorSpace[Array[T], T]): Tej[T] = {
//     new Tej(spire.math.cosh(real), spire.math.sinh(real) *: infinitesimal)
//   }

//   /** tan(a + du) ~= tan(a) + (1 + tan(a)**2) du
//     */
//   def tan(implicit
//       f: Field[T],
//       t: Trig[T],
//       v: VectorSpace[Array[T], T]
//   ): Tej[T] = {
//     val tan_a = spire.math.tan(real)
//     val tmp = f.one + tan_a * tan_a
//     new Tej(tan_a, tmp *: infinitesimal)
//   }

//   /** tanh(a + du) ~= tanh(a) + (1 - tanh(a)**2) du
//     */
//   def tanh(implicit
//       f: Field[T],
//       t: Trig[T],
//       v: VectorSpace[Array[T], T]
//   ): Tej[T] = {
//     val tanh_a = spire.math.tanh(real)
//     val tmp = f.one - tanh_a * tanh_a
//     new Tej(tanh_a, tmp *: infinitesimal)
//   }

//   // Stuff needed by ScalaNumber
//   override def floatValue: Float = doubleValue.toFloat
//   override def doubleValue: Double = anyToDouble(real)
//   override def byteValue: Byte = longValue.toByte
//   override def shortValue: Short = longValue.toShort
//   override def intValue: Int = longValue.toInt
//   override def longValue: Long = anyToLong(real)
//   override def underlying: Object = this
//   override def isWhole: Boolean = anyIsWhole(real) && isReal
//   override def isValidInt: Boolean = anyIsValidInt(real) && isReal

//   // Object stuff
//   override def hashCode: Int = {
//     if (isReal) real.##
//     else 13 * real.## + infinitesimal.raw.foldLeft(53)((x, y) => x + y.## * 19)
//   }

//   override def equals(that: Any): Boolean = that match {
//     case that: Tej[_] => this === that
//     case that         => isReal && real == that
//   }

//   def ===(that: Tej[?]): Boolean =
//     real == that.real && dimension == that.dimension &&
//       infinitesimal.raw.zip(that.infinitesimal.raw).forall { case (x, y) =>
//         x == y
//       }

//   def =!=(that: Tej[?]): Boolean =
//     !(this === that)

//   override def toString: String = {
//     "(%s + [%s]h)".format(real.toString, infinitesimal.toString())
//   }
// }

// trait TejInstance {
//   implicit def TejAlgebra[@sp(Float, Double) T](implicit
//       c: ClassTag[T],
//       d: TejDim,
//       f: Field[T],
//       n: NRoot[T],
//       o: Order[T],
//       s: Signed[T],
//       t: Trig[T]
//   ): TejAlgebra[T] = {
//     import spire.std.array.ArrayVectorSpace
//     new TejAlgebra[T]
//   }

//   implicit def TejEq[T: Eq]: Eq[Tej[T]] = new TejEq[T]
// }

// trait TejIsRing[@sp(Float, Double) T] extends Ring[Tej[T]] {
//   implicit def c: ClassTag[T]
//   implicit def d: TejDim
//   implicit def eq: Eq[T]
//   implicit def f: Field[T]
//   implicit def s: Signed[T]
//   implicit def t: Trig[T]
//   implicit def v: VectorSpace[Array[T], T]

//   override def minus(a: Tej[T], b: Tej[T]): Tej[T] = a - b
//   def negate(a: Tej[T]): Tej[T] = -a
//   def one: Tej[T] = Tej.one[T](c, d, f)
//   def plus(a: Tej[T], b: Tej[T]): Tej[T] = a + b
//   override def pow(a: Tej[T], b: Int): Tej[T] = a.pow(b)
//   override def times(a: Tej[T], b: Tej[T]): Tej[T] = a * b
//   def zero: Tej[T] = Tej.zero(c, d, f)

//   override def fromInt(n: Int): Tej[T] = Tej.fromInt[T](n)
// }

// /* TODO: Tej[T] is probably not a genuine GCD ring */
// trait TejIsGCDRing[@sp(Float, Double) T]
//     extends TejIsRing[T]
//     with GCDRing[Tej[T]] {
//   /* TODO: What exactly is this GCD trying to achieve? Tests? */
//   /*def gcd(a: Tej[T], b: Tej[T]): Tej[T] = {
//     @tailrec def _gcd(a: Tej[T], b: Tej[T]): Tej[T] =
//       if (b.isZero) a else _gcd(b, a - (a / b).round * b)
//     _gcd(a, b)
//   }*/
// }

// /* TODO: Tej[T] is probably not a genuine Euclidean ring */
// trait TejIsEuclideanRing[@sp(Float, Double) T]
//     extends TejIsGCDRing[T]
//     with EuclideanRing[Tej[T]] {
//   /*def euclideanFunction(a: Tej[T]): BigInt = sys.error("Clarify Tej first, see #598")
//   /* TODO: what are exactly the laws of Tej with respect to EuclideanRing ? */
//   def quot(a: Tej[T], b: Tej[T]): Tej[T] = a /~ b
//   def mod(a: Tej[T], b: Tej[T]): Tej[T] = a % b
//   override def quotmod(a: Tej[T], b: Tej[T]): (Tej[T], Tej[T]) = a /% b*/
// }

// /* TODO: Tej[T] is probably not a genuine Field */
// trait TejIsField[@sp(Float, Double) T]
//     extends TejIsEuclideanRing[T]
//     with Field[Tej[T]] {
//   /* TODO: what are exactly the laws of Tej with respect to EuclideanRing ? */
//   // duplicating methods because super[..].call does not work on 2.10 and 2.11
//   override def fromDouble(n: Double): Tej[T] = Tej(f.fromDouble(n))
//   def div(a: Tej[T], b: Tej[T]): Tej[T] = a / b
// }

// trait TejIsTrig[@sp(Float, Double) T] extends Trig[Tej[T]] {
//   implicit def c: ClassTag[T]
//   implicit def d: TejDim
//   implicit def f: Field[T]
//   implicit def n: NRoot[T]
//   implicit def s: Signed[T]
//   implicit def t: Trig[T]
//   implicit def v: VectorSpace[Array[T], T]

//   def e: Tej[T] = Tej(t.e)
//   def pi: Tej[T] = Tej(t.pi)

//   def exp(a: Tej[T]): Tej[T] = a.exp
//   def expm1(a: Tej[T]): Tej[T] = a.exp - f.one
//   def log(a: Tej[T]): Tej[T] = a.log
//   def log1p(a: Tej[T]): Tej[T] = (a + f.one).log

//   def sin(a: Tej[T]): Tej[T] = a.sin
//   def cos(a: Tej[T]): Tej[T] = a.cos
//   def tan(a: Tej[T]): Tej[T] = a.tan

//   def asin(a: Tej[T]): Tej[T] = a.asin
//   def acos(a: Tej[T]): Tej[T] = a.acos
//   def atan(a: Tej[T]): Tej[T] = a.atan
//   def atan2(y: Tej[T], x: Tej[T]): Tej[T] = y.atan2(x)

//   def sinh(x: Tej[T]): Tej[T] = x.sinh
//   def cosh(x: Tej[T]): Tej[T] = x.cosh
//   def tanh(x: Tej[T]): Tej[T] = x.tanh

//   def toRadians(a: Tej[T]): Tej[T] = a
//   def toDegrees(a: Tej[T]): Tej[T] = a
// }

// trait TejIsNRoot[T] extends NRoot[Tej[T]] {
//   implicit def f: Field[T]
//   implicit def n: NRoot[T]
//   implicit def o: Order[T]
//   implicit def t: Trig[T]
//   implicit def s: Signed[T]
//   implicit def c: ClassTag[T]
//   implicit def v: VectorSpace[Array[T], T]

//   def nroot(a: Tej[T], k: Int): Tej[T] = a.nroot(k)
//   override def sqrt(a: Tej[T]): Tej[T] = a.sqrt
//   def fpow(a: Tej[T], b: Tej[T]): Tej[T] = a.pow(b)
//   def fpow(a: T, b: Tej[T]): Tej[T] = b.powScalarToTej(a)
// }

// @SerialVersionUID(0L)
// class TejEq[T: Eq] extends Eq[Tej[T]] with Serializable {
//   def eqv(x: Tej[T], y: Tej[T]): Boolean = x.eqv(y)
//   override def neqv(x: Tej[T], y: Tej[T]): Boolean = x.neqv(y)
// }

// /* TODO
// trait TejIsSigned[T] extends Signed[Tej[T]] {
//   implicit def f: Field[T]
//   implicit def r: IsReal[T]
//   implicit def t: Trig[T]
//   implicit def v: VectorSpace[Array[T],T]

//   def signum(a: Tej[T]): Int = a.signum
//   def abs(a: Tej[T]): Tej[T] = a.abs
//   def compare(x: Tej[T], y: Tej[T]): Int = ???
// }
//  */

// @SerialVersionUID(0L)
// class TejAlgebra[@sp(Float, Double) T](implicit
//     val c: ClassTag[T],
//     val d: TejDim,
//     val eq: Eq[T],
//     val f: Field[T],
//     val n: NRoot[T],
//     val o: Order[T],
//     val t: Trig[T],
//     val s: Signed[T],
//     val v: VectorSpace[Array[T], T]
// ) extends TejIsField[T]
//     with TejIsTrig[T]
//     with TejIsNRoot[T]
// //  with TejIsSigned[T]
//     with VectorSpace[Tej[T], T]
//     with FieldAssociativeAlgebra[Tej[T], T]
//     with Serializable {
//   def scalar: Field[T] = f
//   def nroot: NRoot[T] = n
//   def timesl(a: T, w: Tej[T]): Tej[T] = Tej(a) * w
//   def dot(x: Tej[T], y: Tej[T]): T = ???
//   // {
//   //   x.infinitesimal
//   //     .zip(y.infinitesimal)
//   //     .foldLeft { scalar.times(x.real, y.real) } { (xx, yy) =>
//   //       scalar.plus(xx, scalar.times(yy._1, yy._2))
//   //     }
//   // }
// }
