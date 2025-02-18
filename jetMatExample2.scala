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

@main def jetMatExample2 =
  import spire.math.Jet.*

  // given jmat: JetMatDim = JetMatDim((1, 2))

  extension [T: Numeric](a: Array[T])
    def jetArr(using jd: JetDim): Array[Jet[Double]] =
      a.zipWithIndex.map((v, i) => Jet(v.toDouble) + Jet.h[Double](i))

    def tejArr(using
        jd: TejDim[Double],
        ct: ClassTag[Double]
    ): Array[Tej[Double]] =
      a.zipWithIndex.map(d =>
        Tej(d._1.toDouble)
          + Tej.h(d._2)
      )

  val upper = 2
  val range = (1 to upper.pow(2)).toArray.map(i => i * 4)
  given jd: JetDim = JetDim(upper.pow(2))

  given tejD: TejDim[Double] = TejDim[Double](upper.pow(2))

  given tc: JetMatrixTypeClasses[Jet] = JetMatrixTypeClasses.matJetTC
  given tcTej: JetMatrixTypeClasses[Tej] = TejMatrixTypeClasses.matTejTC

  def gVecMat[F[_]: JetMatrixTypeClasses](
      x: Matrix[F[Double]]
  )(using jd: JetDim, r: Ring[F[Double]]): F[Double] =
    (x).exp.sum

  val x = Matrix[Jet[Double]](
    range.jetArr,
    (upper, upper)
  )
  val xT = Matrix[Tej[Double]](
    range.tejArr,
    (upper, upper)
  )

  println("MATRIX2 -------")
  val out = gVecMat[Jet](x)
  val outT = gVecMat[Tej](xT)

  println("input: x ")

  x.pt

  println

  println(out.toString())

  println(tejD.dag.toGraphviz)

  val backward = outT.backward
  // println(tejD.dag.getAllNodes.mkString("\n"))
  // println(tejD.dag.getAllEdges.mkString("\n"))
  println(backward.size)
  println(
    backward.zipWithIndex
      .map((n, i) => (n.id, n.grad, n.realValue))
      .mkString("\n")
  )

  println
  println(outT.toString())

  println("Nodes of interest; ")

  val nodeInterests = xT.raw.flatMap(n => backward.find(_.id == n.nodeId))
  println(nodeInterests.mkString("\n"))
