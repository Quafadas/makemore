import io.github.quafadas.spireAD.*

import spire._
import spire.math._
import spire.implicits.*
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

import vecxt.all.fromRows
import cats.kernel.Order
import _root_.algebra.ring.Signed
import spire.algebra.VectorSpace
import spire.std.ArrayVectorSpace
import narr.*
import _root_.algebra.ring.Ring
import vecxt.all.update
import vecxt.all.apply
import vecxt.*
import spire.math.Jet.*

@main def jetMatExample2 =

  extension [T: Numeric](a: Array[T])
    def jetArr(using jd: JetDim): Array[Jet[Double]] =
      a.zipWithIndex.map((v, i) => Jet(v.toDouble) + Jet.h[Double](i))

    def tejArr(using
        jd: TejDim[Double],
        ct: ClassTag[Double]
    ): Array[Tej[Double]] =
      a.zipWithIndex.map(d =>
        Tej(d._1.toDouble)
        // + Tej.h(d._2)
      )

  val upper = 2
  val range = (1 to upper.pow(2)).toArray.map(i => i * 4)
  given jd: JetDim = JetDim(upper.pow(2))

  given tejD: TejDim[Double] = TejDim[Double](1)

  given tc: JetMatrixTypeClasses[Jet] = JetMatrixTypeClasses.matJetTC
  given tcTej: JetMatrixTypeClasses[Tej] = TejMatrixTypeClasses.matTejTC

  def gVecMat[F[_]: JetMatrixTypeClasses](
      x: Matrix[F[Double]]
  )(using jd: JetDim, r: Ring[F[Double]]): F[Double] =
    (x @@ x).log.sum

  val x = Matrix[Jet[Double]](
    range.jetArr,
    (upper, upper)
  )
  val xT = Matrix[Tej[Double]](
    range.tejArr,
    (upper, upper)
  )

  println("Forward Pass -------")
  val out = gVecMat[Jet](x)
  val outT = gVecMat[Tej](xT)

  println("input: x ")

  xT.pt

  println

  println("This is pure spire forward mode - :-) I trust it.")
  println(out.toString())

  // if you want to see the calcualtion graph
  // println(tejD.dag.toGraphviz)

  println(
    "We did the forward pass above, but _without_ calculating the gradients - TejDim was set to zero and there's no infintesimals in the input."
  )
  val backward = outT.backward

  println
  // println(outT.toString())

  println(
    "Let's haul the results out of the backward pass. Nodes of interest; "
  )

  val nodeInterests = xT.raw.flatMap(n => backward.find(_.id == n.nodeId))
  println(nodeInterests.mkString("\n"))
