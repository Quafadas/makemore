import cats._

import vecxt.all.*
import spire.math.Jet
import spire.math.JetDim
import spire.math.JetAlgebra

// Define the Id type alias
type Id[A] = A

type JetArray[T] = Jet[Array[T]]

trait Reducible[F[_], T] {
  extension (a: F[T])
    def sum: T
    def product: T
}

object Reducible {

  given rd: Reducible[Id, Double] =
    new Reducible[Id, Double] {
      extension (a: Id[Double]) {
        def sum: Double = a
        def product: Double = a
      }
    }

  given rjd: Reducible[Id, Jet[Double]] =
    new Reducible[Id, Jet[Double]] {
      extension (a: Id[Jet[Double]]) {
        def sum: Jet[Double] = a
        def product: Jet[Double] = a
      }
    }

  given rtd: Reducible[Id, Tej[Double]] =
    new Reducible[Id, Tej[Double]] {
      extension (a: Id[Tej[Double]]) {
        def sum: Tej[Double] = a
        def product: Tej[Double] = a
      }
    }

  given rad: Reducible[Array, Double] =
    new Reducible[Array, Double] {
      extension (arrD: Array[Double]) {
        def sum: Double = vecxt.arrays.sum(arrD)
        def product: Double = arrD.foldLeft(1.0)(_ * _)
      }
    }

  given radj(using jd: JetDim): Reducible[Array, Jet[Double]] =
    new Reducible[Array, Jet[Double]] {
      import spire.implicits.DoubleAlgebra
      import JetArrayTypeClasses.arrjAD
      extension (arrD: Array[Jet[Double]]) {
        def sum: Jet[Double] = arrD.foldLeft(Jet(0.0))(_ + _)
        def product: Jet[Double] = arrD.foldLeft(Jet(1.0))(_ * _)
      }
    }

  given rarrj(using jd: JetADDim): Reducible[cats.Id, JetAD] =
    new Reducible[cats.Id, JetAD] {
      inline given n: vecxt.BoundsCheck.BoundsCheck =
        vecxt.BoundsCheck.DoBoundsCheck.yes
      extension (a: Id[JetAD])
        override def sum: JetAD = {
          val sumI =
            for (i <- 0 until a.infinitesimal.cols)
              yield a.infinitesimal.col(i)
          val summed =
            sumI.foldLeft(Array.fill[Double](a.infinitesimal.rows)(0.0))(_ + _)
          val rc = (a.infinitesimal.rows, 1)
          JetAD(
            Array(a.real.sum),
            Matrix(summed, rc)
          )

        }
        override def product: JetAD =

          val p = a.real.product
          val productI =
            for i <- 0 until a.infinitesimal.cols
            yield a.infinitesimal.col(i) * (p / a.real(i))
          val rc = (a.infinitesimal.rows, 1)
          val arr = productI.foldLeft(
            Array.fill[Double](a.infinitesimal.rows)(0.0)
          )(_ + _)
          val mat: Matrix[Double] = Matrix(arr, rc)
          JetAD(
            Array(p),
            mat
          )

    }

}

trait Reduciblely[F[_], G[_], T] {
  extension (a: F[G[T]])
    def sum: G[T]
    def product: G[T]
}

object Reduciblely {
  given rd: Reduciblely[Id, Id, Double] =
    new Reduciblely[Id, Id, Double] {
      extension (a: Id[Id[Double]]) {
        def sum: Id[Double] = a
        def product: Id[Double] = a
      }
    }

  given rjd: Reduciblely[Id, Id, Jet[Double]] =
    new Reduciblely[Id, Id, Jet[Double]] {
      extension (a: Id[Id[Jet[Double]]]) {
        def sum: Id[Jet[Double]] = a
        def product: Id[Jet[Double]] = a
      }
    }

  given rtd: Reduciblely[Id, Id, Tej[Double]] =
    new Reduciblely[Id, Id, Tej[Double]] {
      extension (a: Id[Id[Tej[Double]]]) {
        def sum: Id[Tej[Double]] = a
        def product: Id[Tej[Double]] = a
      }
    }

  given rad: Reduciblely[Array, Id, Double] =
    new Reduciblely[Array, Id, Double] {
      extension (arrD: Array[Id[Double]]) {
        def sum: Id[Double] = vecxt.arrays.sum(arrD.map(identity))
        def product: Id[Double] = arrD.foldLeft(1.0)(_ * _)
      }
    }

  given radj(using jd: JetDim): Reduciblely[Array, Id, Jet[Double]] =
    new Reduciblely[Array, Id, Jet[Double]] {
      import spire.implicits.DoubleAlgebra
      import JetArrayTypeClasses.arrjAD
      extension (arrD: Array[Id[Jet[Double]]]) {
        def sum: Id[Jet[Double]] = arrD.foldLeft(Jet(0.0))(_ + _)
        def product: Id[Jet[Double]] = arrD.foldLeft(Jet(1.0))(_ * _)
      }
    }

}
