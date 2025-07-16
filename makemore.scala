import io.github.quafadas.table.*

// import viz.PlotNt.plot
import org.apache.commons.math3.distribution.EnumeratedIntegerDistribution
import io.github.quafadas.spireAD.*
import scala.reflect.ClassTag
// import scala.NamedTuple
// import scala.NamedTuple.*
// import vecxt.all.*
// import viz.NamedTupleReadWriter.given
import vecxt.arrays.*
import viz.PlotTargets.doNothing
import viz.FromResource
import viz.PlotTarget
// import vecxt.*
import viz.PlotTargets
import pprint.*
import scala.collection.View
import scala.collection.immutable.HashMap
import vecxt.BoundsCheck.DoBoundsCheck.yes
import scala.util.chaining.*

import io.github.quafadas.spireAD.*
import spire.*
import spire.implicits.*
import spire.algebra.*
import vecxt.matrix.Matrix
import vecxt.all.*
import scala.reflect.ClassTag
import vecxt.all.row
import cats.syntax.all.toShow

type JsonMod = ujson.Value => Unit
case class HeatmapBigram(override val mods: Seq[JsonMod] = List())(using
    PlotTarget
) extends FromResource:
  override lazy val path = "HeatmapBigram.vg.json"

def heatmap(data: collection.Seq[(String, Int)])(using PlotTarget) =
  HeatmapBigram(
    Seq(
      (spec: ujson.Value) =>
        spec("data")("values") = data.map((a, b) =>
          ujson.Obj(
            "first" -> a.charAt(0).toString(),
            "second" -> a.charAt(1).toString(),
            "value" -> b
          )
        ),
      viz.Utils.fillDiv
    )
  )

def onehot(char: Char, allChars: collection.Map[Char, Int]): Array[Double] =
  val idx2 = allChars(char)
  Array.fill(allChars.size)(0.0).tap(_(idx2) = 1.0)

// @main def checkOneHot =
//   val chars = ('a' to 'z').toVector
//   val allChars = chars.zipWithIndex.toMap
//   val char = 'c'
//   val onehotChar = onehot(char, allChars)
//   println(onehotChar.printArr)

import cats.Show

object detailShow:
  given Show[Matrix[Double]] with
    def show(matrix: Matrix[Double]): String =
      val rows =
        for i <- 0 until Math.min(1, matrix.rows)
        yield matrix
          .row(i)
          .map(s => "%.3f".format(s).reverse.padTo(6, ' ').reverse)
          .mkString(" | ")
      val footer = ("-" * (rows.head.length))
      (rows :+ footer).mkString("\n")

  given Show[Array[Double]] with
    def show(arr: Array[Double]): String =

      arr.map("%.3f".format(_).reverse.padTo(6, ' ').reverse).mkString("[", ", ", "]")

  given Show[Scalar[Double]] with
    def show(arr: Scalar[Double]): String =
      arr.scalar.toString

object LiteShow:
  given Show[Matrix[Double]] with
    def show(matrix: Matrix[Double]): String =
      "Mat"

  given Show[Array[Double]] with
    def show(arr: Array[Double]): String =
      "arr"

  given Show[Scalar[Double]] with
    def show(arr: Scalar[Double]): String =
      "%.3f".format(arr.scalar)

def graphDebug(s: String) =
  os.write.over(os.Path("/Users/simon/Code/makemore") / "graph.dot", s)


def saveMatrixToCSV(matrix: Matrix[Double], filePath: String): Unit =
  val lines = for (i <- 0 until matrix.rows) yield matrix.row(i).mkString(",")
  os.write.over(os.Path(filePath, os.pwd), lines.mkString("\n"))

def loadMatrixFromCSV(filePath: String)(using ClassTag[Double]): Matrix[Double] =
  val lines = os.read.lines(os.Path(filePath, os.pwd))
  val data = lines.map(_.split(",").map(_.toDouble)).toArray
  Matrix.fromRowsArray(data)


@main def makemore: Unit =

  val generateWeights = false
  import LiteShow.given

  val normalDist =
    new org.apache.commons.math3.distribution.NormalDistribution()

  val smooth = true

  val chars = '.' +: ('a' to 'z').toVector
  val charsIdx = chars.zipWithIndex.toMap

  def data: CsvIterator[Tuple1["name"]] = CSV.resource("names.txt")

  /** Bookended : adds "."to either end of the string Pairs : extracts the
    * series of pairs of the bookended characters characters Ints : indexes the
    * bookended characters xenc : one hot encodes the characters excluding the
    * last character
    */
  def bookended = data
    .addColumn["Bookend", String](s => s".${s.name}.")
    .addColumn["Pairs", Seq[String]](s => s.Bookend.sliding(2).toSeq)
    .addColumn["Ints", Seq[Int]](s => s.Bookend.map(charsIdx.seq))
    .addColumn["xenc", Seq[Array[Double]]](s =>
      s.Ints.init.map(i => onehot(chars(i), charsIdx.seq))
    )
    .addColumn["yenc", Array[Int]](s => s.Ints.tail.toArray)
    // .take(10)

  val combinations = for {
    a <- chars
    b <- chars
  } yield s"$a$b"

  println("Change lange to neural network")
  println(s"Check Data")
  println(bookended.take(5).mkString("\n"))

  val dimensions = 27 * 27
  val randoms = Array.fill(dimensions)(normalDist.sample())

  val W = if (generateWeights)
    val tmp = Matrix(randoms, (27, 27))
    saveMatrixToCSV(tmp, "weights.csv")
    tmp
  else
    loadMatrixFromCSV("weights.csv")

  val xnencM =
    bookended
      // .take(useFirstn)
      .flatMap(_.xenc)

  val xencMall: Matrix[Double] = Matrix.fromRowsArray(
    xnencM.toArray
  )

  println(xencMall.shape)
  val yChars =
    bookended.flatMap(_.yenc).toArray

  inline def calcLoss(
      weights: Matrix[Double],
      incomingData: Matrix[Double],
      targets: Array[Int]
  )(using
      inline mOps: Matrixy[Matrix, Double],
      inline fm: VectorisedField[Matrix, Double],
      inline fa: VectorisedTrig[Array, Double],
      inline fas: VectorisedField[Scalar, Double],
      inline faa: VectorisedField[Array, Double],
      inline redArr: Reductions[Array, Double, 1],
      inline redMat: Reductions[Matrix, Double, 2],
      inline t: VectorisedTrig[Matrix, Double],
      nt: Numeric[Double],
      ct: ClassTag[Double]
  ): Scalar[Double] =
    val logits = incomingData @@ weights
    val counts = logits.exp
    val probsNN = counts.mapRows(row => row / row.sum)
    val range = (0 until targets.length).toArray.zip(targets)
    -Scalar(probsNN(range).mapRowsToScalar(_.sum).log.mean)

  val loss = calcLoss(W, xencMall, yChars)
  println(loss)

  inline def calcLossF(
      weights: TejV[Matrix, Double],
      incomingData: TejV[Matrix, Double],
      targets: Array[Int]
  )(using
      inline mOps: Matrixy[Matrix, Double],
      inline fm: VectorisedField[Matrix, Double],
      inline fa: VectorisedField[Array, Double],
      inline fas: VectorisedField[Scalar, Double],
      fi: Field[Double],
      // inline redArr: Reductions[F, T, 1],
      inline redMat: Reductions[Matrix, Double, 2],
      inline redArr: Reductions[Array, Double, 1],
      inline vtm: VectorisedTrig[Matrix, Double],
      inline vta: VectorisedTrig[Array, Double],
      inline vts: VectorisedTrig[Scalar, Double],
      inline dag: TejVGraph[Double],
      inline nt: Numeric[Double],
      inline ct: ClassTag[Double],
      inline sh: Show[Matrix[Double]],
      inline sha: Show[Array[Double]],
      inline shs: Show[Scalar[Double]]
  ): TejV[Scalar, Double] =

    val logits = incomingData @@ weights

    val probsNN = logits.softmaxRows
    // println(s"ProbsNN: ${probsNN.value(Array(0,1), ::).printMat}")
    val range: Array[(Int, Int)] = (0 until targets.length).toArray.zip(targets)
    // println(s"Range: ${range.take(5).mkString(", ")}")
    // val selected = probsNN(range)
    // println(s"Selected: ${selected.value(Array(0,1), ::).printMat}")
    // val mat: Matrix[Double] = selected.value
    val selected = probsNN.arrange(range)
    // println(s"Selected: ${selected.value.take(10).mkString(", ")}")

    val theloss = (  selected + 1e-8d.tej).log.mean * TejV(Scalar(fi.fromDouble(-1.0)))
    theloss

  @annotation.tailrec
  def train(
    weights: Matrix[Double],
    data: Matrix[Double],
    targets: Array[Int],
    learningRate: Double,
    steps: Int
  ): TejV[Matrix, Double] =
    given graph: TejVGraph[Double] = TejVGraph[Double]()
    val weights_ = TejV(weights)
    val data_ = TejV(data)

    if steps == 0 then
        weights_
    else
      val loss = calcLossF(weights_, data_, targets)
      val grad = loss.backward2((weights = weights_))
      val updated = weights - (grad.weights * learningRate)
      if steps % 10 == 0 then
        println(s"Step $steps, loss: ${loss.value.scalar}, rate: $learningRate")
      if steps - 1 == 0  then
        graphDebug(graph.dag.toGraphviz)
        println(s"Final loss: ${loss.value.scalar}")

      val learningRate_ = if steps % 20 == 0 then learningRate - 0.1 else learningRate

      // Tail recursive: last call is train
      train(updated, data, targets, learningRate_, steps - 1)

  def generate(
      inChar: Char,
      weights: Matrix[Double],
      charDist: Map[Char, EnumeratedIntegerDistribution],
      chars: Seq[(Char, Int)]
  ) =
    ???
    // Iterator.unfold[Char, Char]('.') { c =>
    //   // println(s"generate $c")
    //   val nextChar = charDist.get(c) match
    //     case Some(d) =>
    //       val next = d.sample()
    //       // println(next)
    //       inChars(next)
    //     case None => '.'

    //   // println(nextChar)
    //   nextChar match
    //     case '.' => None
    //     case newChar =>
    //       Some(newChar -> newChar)
    // }


  println("Training the model...")

  println(s"Initial weights shape: ${W.shape}")
  println(s"Input data shape: ${xencMall.shape}")
  println(s"Target data shape: ${yChars.length}")
  println(s"First 5 target characters: ${yChars.take(5).mkString(", ")}")


  val learningRate = 1
  val steps = 100
  val weightsTrained = train(W, xencMall, yChars, learningRate, steps)


  println("initial loss")
  println(loss)

  saveMatrixToCSV(weightsTrained.value, "weights.csv")

  println("training run finished")


