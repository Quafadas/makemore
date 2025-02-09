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

}
