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
import viz.PlotTargets.desktopBrowser

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


@main def makemore_neural_final: Unit =

  val generateWeights = true
  import LiteShow.given

  val normalDist = new org.apache.commons.math3.distribution.NormalDistribution()
  val smooth = true

  val chars = '.' +: ('a' to 'z').toVector
  val charsIndex = (0 to 26).toVector
  val charsMap = chars.zip(charsIndex).toMap
  val i2c = charsIndex.zip(chars).toMap

  val data = CSV.absolutePath("/Users/simon/Code/makemore/data/names.txt")

  /** Bookended : adds "."to either end of the string Pairs : extracts the
    * series of pairs of the bookended characters characters Ints : indexes the
    * bookended characters xenc : one hot encodes the characters excluding the
    * last character
    */
  val bookended = LazyList.from(
    data
    .addColumn["Bookend", String](s => s".${s.name}.")
    .addColumn["Pairs", Seq[String]](s => s.Bookend.sliding(2).toSeq)
    .addColumn["Ints", Seq[Int]](s => s.Bookend.map(charsMap.seq))
  )

  println("Change lange to neural network")
  println(s"Check Data")
  println(bookended.take(5).mkString("\n"))

  val trainData = bookended
    .flatMap(_.Pairs)
    .map(p => (first = p.head, last = p.last))
    .addColumn["target", Int](row => charsMap(row.last))
    .addColumn["xenc", Array[Double]](row => onehot(row.first, charsMap.seq))
    .toArray

  trainData.take(5).mapColumn["xenc", String](_.mkString("[", ",", "]")).toSeq.ptbln


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

  val yChars = trainData.column["target"]

  /**
   * Calculates the cross-entropy loss for a neural network classification task.
   *
   * This method computes the negative log-likelihood loss by:
   * 1. Computing logits through matrix multiplication of data and weights
   * 2. Applying softmax normalization row-wise to convert logits to probabilities
   * 3. Selecting the probabilities corresponding to the target classes
   * 4. Computing the mean negative log-probability with numerical stability (epsilon = 1e-8)
   *
   * @param weights The weight matrix to apply to the input data
   * @param data The input data matrix
   * @param targets Array of target class indices (one per sample)
   * @param mOps Implicit loss context operations for matrix computations
   * @param tg Implicit computational graph for automatic differentiation
   * @param fi Implicit fractional operations for Double type
   * @return The scalar loss value (cross-entropy loss)
   */

  inline def calcLossF(
      weights: TejV[Matrix, Double],
      data: TejV[Matrix, Double],
      targets: Array[Int]
  )(using
      inline mOps: LossContext[Matrix, Array, Scalar, Double],
      tg: TejVGraph[Double],
      fi: Fractional[Double]
  ): TejV[Scalar, Double] =
    val logits = data @@ weights
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
      steps: Int,
      initialSteps: Int,
      initialLearningRate: Double,
      epochLength: Int = 50
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

      if (steps - 1) % 10 == 0 then
        println(s"Step $steps, loss: ${loss.value.scalar}, rate: $learningRate")
        heatmap(updated.exp, chars.toArray, charsIndex.toArray, "NN: Coarse training")
      if steps - 1 == 0 then
        graphDebug(graph.dag.toGraphviz)
        println(s"Final loss: ${loss.value.scalar}")
      if steps % 100 == 0 then
        println("saving weights")
        saveMatrixToCSV(updated, "weights.csv")


      val learningRate_ = if steps % epochLength == 0 then
        initialLearningRate * (1 / (1 + ((initialSteps - steps) / 20.0).toDouble ))
      else
        learningRate

      train(updated, data, targets, learningRate_, steps - 1, initialSteps, initialLearningRate)


  @annotation.tailrec
  def train2(
      weights: Matrix[Double],
      data: Matrix[Double],
      targets: Array[Int],
      learningRate: Double,
      steps: Int,
      initialSteps: Int,
      initialLearningRate: Double,
      epochLength: Int = 50
  ): Matrix[Double] =
    var updated = weights.deepCopy

    if steps == 0 then weights
    else
      for (i <- 0 to data.rows by 500) {
        given graph: TejVGraph[Double] = TejVGraph[Double]()
        val weights_ = weights.tej
        val maxIdx = Math.min(i + 500, data.rows)
        val range = Range(i, maxIdx)
        val dataSlice = data(range, ::)
        val targetsSlice = targets.slice(i, maxIdx)
        val loss = calcLossF(weights_, dataSlice.tej, targetsSlice)
        val grad = loss.backward((weights = weights_))
        grad.weights *= learningRate
        updated -= grad.weights
      }

      if (steps - 1) % 10 == 0 then
        given graph: TejVGraph[Double] = TejVGraph[Double]()
        val loss = calcLossF(updated.tej, data.tej, targets)
        println(s"Step $steps, loss: ${loss.value.scalar}, rate: $learningRate")
        heatmap(updated.exp, chars.toArray, charsIndex.toArray, s"NN: Small batch update ${initialSteps - steps}")

      val learningRate_ = if steps % epochLength == 0 then
        initialLearningRate * (1 / (1 + ((initialSteps - steps) / 20.0).toDouble ))
      else
        learningRate

      // Tail recursive: last call is train
      train2(updated, data, targets, learningRate_, steps - 1, initialSteps, initialLearningRate)


  println("-- Begin Training run...")

  println(s"Initial weights shape: ${W.shape}")
  println(s"Input data shape: ${xencMall.shape}")
  println(s"Target data shape: ${yChars.length}")
  println(s"First 5 target characters: ${yChars.take(5).mkString(", ")}")

  val learningRate = 0.1
  val steps = 100
  val trainStart = train(W, xencMall, yChars.toArray, learningRate, 2, 2, learningRate)
  val weightsTrained = train2(trainStart.value, xencMall, yChars.toArray, learningRate, steps, steps, learningRate)
  // val weightsTrained = train(W, xencMall, yChars.toArray, learningRate, steps, steps, learningRate)

  println("initial loss")
  // println(loss)

  // saveMatrixToCSV(weightsTrained.value, "weights.csv")

  println("training run finished")
