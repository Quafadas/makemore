import spire._
import spire.math._
import spire.implicits._
import spire.algebra.Trig
import _root_.algebra.ring.Field

import vecxt.arrays.*
import scala.Array.ArrayFactory
import vecxt.BoundsCheck.*
import vecxt.BoundsCheck.DoBoundsCheck.yes
import vecxt.dimMatCheck
import spire.algebra.NRoot
import vecxt.matrix.Matrix
import vecxt.all.eye
import vecxt.all.ones

@main def jetAd_example =

  // given nroot: NRoot[Array[Double]] = JetAd_TC.nrootAD

  // given jetARing: JetADIsRing = new JetADIsRing {

  //   override implicit def fd: Field[Double] = ???

  //   override implicit def fad: Field[Array[Double]] = JetAd_TC.fadAD

  //   override implicit def d: JetADDim = jd2

  // }
  // given jetField: JetADIsField = new JetADIsField {

  //   override implicit def fd: Field[Double] = ???

  //   override implicit def fad: Field[Array[Double]] = JetAd_TC.fadAD

  //   override implicit def d: JetADDim = jd2

  // }

  // given jd: JetDim = JetDim(2)
  // val x: Jet[Double] = 2.0 + Jet.h[Double](0)
  // val y: Jet[Double] = 3.0 + Jet.h[Double](1)

  // def f[@specialized(Double) T: Field](x: T, y: T): T = x * x + x * y

  // def soA(
  //     x: Array[Double],
  //     y: Array[Double]
  // )(using fad: Field[Array[Double]], tad: Trig[Array[Double]]): Array[Double] =
  //   val i = x * y

  //   val j = sin(x)
  //   i + j

  // def g[@specialized(Double) T: Field: Trig](x: T, y: T): T =
  //   sin(log(exp(x) + exp(-y)) - x / cos(y))

  // // https://stats.stackexchange.com/questions/224140/step-by-step-example-of-reverse-mode-automatic-differentiation
  // def so[@specialized(Double) T: Field: Trig](x: T, y: T): T =
  //   x * y + sin(x)

  // def softmax[@specialized(Double) T: Field: Trig](x: T, y: T): T =
  //   x * y /

  // for i <- 0 to 3 do
  //   val x = i + Jet.h[Double](0)
  //   val y = i + Jet.h[Double](1)

  //   val z = so(x, y)
  //   println(z)

  // val result = so(xa, ya)
  // println(result.mkString(", "))

  // val z = so(x, y)

  def sq[T: Field](x: T): T = x * x
  def sqExp[T: Field: Trig](x: T): T =
    (x * x + exp(x) - log(x) + exp(sin(x))) / x

  given jd: JetDim = JetDim(1)
  val simpleJet1 = Jet(1.0) + Jet.h[Double](0)
  val simpleJet2 = Jet(2.0) + Jet.h[Double](0)
  val xa = Array[Double](1, 2, 3)
  val ya = Array[Double](1, 2, 3)

  println("Seuare only")
  println(sq(simpleJet1))
  println(sq(simpleJet2))

  import JetAd_TC.signed
  given jd2: JetADDim = JetADDim((2, 1))

  val xJD: JetAD = JetAD(Array[Double](1.0, 2.0), Matrix.ones[Double]((1, 2)))
  val zAD = sq(xJD)

  println(zAD)
  println("With Exp")

  println(sqExp(simpleJet1))
  println(sqExp(simpleJet2))

  println(sqExp(xJD))
