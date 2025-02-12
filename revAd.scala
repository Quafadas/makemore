import spire._
import spire.math._
import spire.implicits._
import _root_.algebra.ring.Field
import spire.algebra.Trig


@main def revAd =  

  given jd: TejDim = TejDim(2)
  val x: Tej[Double] = 1.0 + Tej.h[Double](0)
  val y: Tej[Double] = 1.0 + Tej.h[Double](1)

  def f[@specialized(Double) T: Field](x: T, y: T): T = x * x + x * y

  def g[@specialized(Double) T: Field: Trig](x: T, y: T): T =
    sin(log(exp(x) + exp(-y)) - x / cos(y))

  // println(f(x, y))
  println(g(x, y))
  println(jd.dag.toGraphviz)
