import io.github.quafadas.table.*
import org.apache.commons.math3.distribution.EnumeratedIntegerDistribution
import io.github.quafadas.inspireRAD.*
import scala.reflect.ClassTag

import vecxt.arrays.*
import viz.PlotTargets.doNothing
import pprint.*
import scala.collection.View
import scala.collection.immutable.HashMap
import vecxt.BoundsCheck.DoBoundsCheck.yes
import scala.util.chaining.*

import spire.*
import spire.implicits.*
import spire.algebra.*
import vecxt.matrix.Matrix
import vecxt.all.*
import scala.reflect.ClassTag
import vecxt.all.row
import cats.syntax.all.toShow
import cats.Show
import io.github.quafadas.inspireRAD.DetailShow
import io.github.quafadas.inspireRAD.LiteShow

def onehot(char: Char, allChars: collection.Map[Char, Int]): Array[Double] =
  val idx2 = allChars(char)
  Array.fill(allChars.size)(0.0).tap(_(idx2) = 1.0)

def graphDebug(s: String) =
  os.write.over(os.Path("/Users/simon/Code/makemore") / "graph.dot", s)

def saveMatrixToCSV(matrix: Matrix[Double], filePath: String): Unit =
  val lines = for (i <- 0 until matrix.rows) yield matrix.row(i).mkString(",")
  os.write.over(os.Path(filePath, os.pwd), lines.mkString("\n"))

def loadMatrixFromCSV(filePath: String)(using
    ClassTag[Double]
): Matrix[Double] =
  val lines = os.read.lines(os.Path(filePath, os.pwd))
  val data = lines.map(_.split(",").map(_.toDouble)).toArray
  Matrix.fromRowsArray(data)


@main def makemore_neural: Unit =

  val generateWeights = false
  import LiteShow.given

  val normalDist =
    new org.apache.commons.math3.distribution.NormalDistribution()

  val smooth = true

  val chars = '.' +: ('a' to 'z').toVector
  val charsIdx = chars.zipWithIndex.toMap

  def data = CSV.resource("names.txt")

  /** Bookended : adds "."to either end of the string Pairs : extracts the
    * series of pairs of the bookended characters characters Ints : indexes the
    * bookended characters xenc : one hot encodes the characters excluding the
    * last character
    */
  def bookended = data
    .addColumn["Bookend", String](s => s".${s.name}.")
    .addColumn["Pairs", Seq[String]](s => s.Bookend.sliding(2).toSeq)
    .addColumn["Ints", Seq[Int]](s => s.Bookend.map(charsIdx.seq))
  // .addColumn["xenc", Seq[Array[Double]]](s =>
  //   s.Ints.init.map(i => onehot(chars(i), charsIdx.seq))
  // )
  // .addColumn["yenc", Array[Int]](s => s.Ints.tail.toArray)

  def trainData =
    bookended
      .flatMap(_.Pairs)
      .map(p => (first = p.head, last = p.last))
      .addColumn["target", Int](row => charsIdx(row.last))
      .addColumn["xenc", Array[Double]](row => onehot(row.first, charsIdx.seq))

      .toArray

  trainData.take(5).mapColumn["xenc", String](_.mkString("[", ",", "]")).toSeq.ptbln

  println(trainData.length)

  val combinations = for {
    a <- chars
    b <- chars
  } yield s"$a$b"

  // val trainData = bookended.flatMap(_.Pairs).map(p =>
  //   (pair = p, target = charsMap(p.last), onehot = onehot(p.head, charsMap).mkString(",") )
  // )

  println("Change lange to neural network")
  println(s"Check Data")
  println(bookended.take(5).mkString("\n"))

  val dimensions = 27 * 27
  val randoms = Array.fill(dimensions)(normalDist.sample())

  val W = if (generateWeights)
    val tmp = Matrix(randoms, (27, 27))
    saveMatrixToCSV(tmp, "weights.csv")
    tmp
  else loadMatrixFromCSV("weights.csv")


  val xencMall: Matrix[Double] = Matrix.fromRowsArray(
    trainData.column["xenc"].toArray
  )

  println(xencMall.shape)
  val yChars = trainData.column["target"]


  inline def calcLossF(
      weights: TejV[Matrix, Double],
      incomingData: TejV[Matrix, Double],
      targets: Array[Int]
  )(using
      inline mOps: LossContext[Matrix, Array, Scalar, Double],
      tg: TejVGraph[Double],      fi: Fractional[Double]
  ): TejV[Scalar, Double] =

    val logits = incomingData @@ weights
    val probsNN = logits.softmaxRows
    val range: Array[(Int, Int)] = (0 until targets.length).toArray.zip(targets)
    val selected = probsNN.arrange(range)
    val theloss = (selected + 1e-8d.tej).log.mean * -1.0.tej
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
    val weights_ = weights.tej
    val data_ = data.tej

    if steps == 0 then weights_
    else
      val loss = calcLossF(weights_, data_, targets)
      val grad = loss.backward((weights = weights_))
      grad.weights *= learningRate

      val updated = weights - (grad.weights)
      if steps % 10 == 0 then
        println(s"Step $steps, loss: ${loss.value.scalar}, rate: $learningRate")
      if steps - 1 == 0 then
        graphDebug(graph.dag.toGraphviz)
        println(s"Final loss: ${loss.value.scalar}")

      val learningRate_ =
        if steps % 20 == 0 then learningRate - 0.1 else learningRate

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
  val weightsTrained = train(W, xencMall, yChars.toArray, learningRate, steps)

  println("initial loss")
  // println(loss)

  saveMatrixToCSV(weightsTrained.value, "weights.csv")

  println("training run finished")
