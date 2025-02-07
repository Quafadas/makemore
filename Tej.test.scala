//> using dep org.scalameta::munit::1.1.0

import munit._
import spire._
import spire.math._
import spire.implicits._
import _root_.algebra.ring.Field
import spire.algebra.Trig
import spire.syntax.rig
import _root_.algebra.ring.Rig
import vecxt.all.*

// import Tej.*
// import scala.math.Fractional.Implicits.infixFractionalOps
// import scala.math.Integral.Implicits.infixIntegralOps
// import scala.math.Numeric.Implicits.infixNumericOps

import vecxt.BoundsCheck.DoBoundsCheck.yes
import spire.algebra.VectorSpace
import spire.algebra.NRoot

class TejSuite extends FunSuite {

  test("Tej addition") {
    given jd: TejDim = TejDim(2)
    val x: Tej[Double] = 1.0 + Tej.h[Double](0)
    val y: Tej[Double] = 1.0 + Tej.h[Double](1)
    val result = x + y
    assertEquals(result, Tej(2.0, Array(1.0, 1.0)))
  }

  test("Tej multiplication") {
    given jd: TejDim = TejDim(2)
    val x: Tej[Double] = 1.0 + Tej.h[Double](0)
    val y: Tej[Double] = 1.0 + Tej.h[Double](1)
    val result = x * y
    assertEquals(result, Tej(1.0, Array(1.0, 1.0)))
  }

  test("Tej equality") {
    val x = Tej(1.0, Array(0.0))
    val y = Tej(1.0, Array(0.0))
    assertEquals(x, y)
  }

  test("zero Tej") {
    given jd: TejDim = TejDim(2)
    val zero = Tej.zero[Double]
    assertEquals(zero, Tej(0.0, Array(0.0, 0.0)))
  }

  test("Array Double") {
    import spire.std.array.*
    given jd: TejDim = TejDim(2)
    // given arrRig: Rig[Array[Double]] = new Rig[Array[Double]] {

    //   override def one: Array[Double] = Array(1.0, 1.0)

    //   def zero: Array[Double] = Array(0.0, 0.0)
    //   def plus(x: Array[Double], y: Array[Double]): Array[Double] =
    //     x.zip(y).map((a, b) => a + b)
    //   def times(x: Array[Double], y: Array[Double]): Array[Double] =
    //     x.zip(y).map((a, b) => a * b)
    // }

    given fieldRig(using td: TejDim): Field[Array[Double]] =
      new Field[Array[Double]] {

        inline override def negate(x: Array[Double]): Array[Double] = x.map(-_)

        inline override def div(
            x: Array[Double],
            y: Array[Double]
        ): Array[Double] =
          x.zip(y).map((a, b) => a / b)

        inline override def one: Array[Double] = Array.fill(td.dimension)(1.0)

        inline def zero: Array[Double] = Array.fill(td.dimension)(0.0)
        inline def plus(x: Array[Double], y: Array[Double]): Array[Double] =
          x + y
        inline def times(x: Array[Double], y: Array[Double]): Array[Double] =
          x.zip(y).map((a, b) => a * b)
      }

    given vs(using td: TejDim): VectorSpace[Array[Double], Double] =
      new VectorSpace[Array[Double], Double] {
        def scalar: Field[Double] = Field[Double]
        def timesl(r: Double, v: Array[Double]): Array[Double] = v * r
        def negate(v: Array[Double]): Array[Double] = v.map(-_)
        def zero: Array[Double] = Array.fill(td.dimension)(0.0)
        def plus(x: Array[Double], y: Array[Double]): Array[Double] = x + y
      }

    // given nRoot: NRoot[Array[Double]] =
    //   new spire.algebra.NRoot[Array[Double]] {}

    val x: Tej[Array[Double]] =
      Tej(Array(1.0, 2.0)) + Tej.h[Array[Double]](Array.fill(2)(0.0))
    x.show

    def square(x: Tej[Array[Double]]): Tej[Array[Double]] =
      x * x

    square(x).show

  }

}
