import io.github.quafadas.table.*

// import viz.PlotNt.plot
import org.apache.commons.math3.distribution.EnumeratedIntegerDistribution
import io.github.quafadas.spireAD.*

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
import spire.implicits.*
import vecxt.matrix.Matrix
import vecxt.all.fromRows
import vecxt.all.fromRowsArray
import vecxt.all.printArr
import scala.Tuple.Append

// @main def checkOneHot =
//   val chars = ('a' to 'z').toVector
//   val allChars = chars.zipWithIndex.toMap
//   val char = 'c'
//   val onehotChar = onehot(char, allChars)
//   println(onehotChar.printArr)

@main def makemore_1: Unit =

  val normalDist =
    new org.apache.commons.math3.distribution.NormalDistribution()

  val smooth = true

  val chars = '.' +: ('a' to 'z').toVector
  val charsIdx = chars.zipWithIndex.toMap

  val combinations = for {
    a <- chars
    b <- chars
  } yield s"$a$b"

  def data = CSV.resource("names.txt")

  /** Bookended : adds "."to either end of the string Pairs : extracts the
    * series of pairs of the bookended characters characters Ints : indexes the
    * bookended characters xenc : one hot encodes the characters excluding the
    * last character
    */
  def bookended: Iterator[
    (
        name: String,
        Bookend: String,
        Pairs: Seq[String],
        Ints: Seq[Int],
        xenc: Seq[Array[Double]],
        yenc: Array[Int]
    )
  ] = data
    .addColumn["Bookend", String](s => s".${s.name}.")
    .addColumn["Pairs", Seq[String]](s => s.Bookend.sliding(2).toSeq)
    .addColumn["Ints", Seq[Int]](s => s.Bookend.map(charsIdx.seq))
    .addColumn["xenc", Seq[Array[Double]]](s =>
      s.Ints.init.map(i => onehot(chars(i), charsIdx.seq))
    )
    .addColumn["yenc", Array[Int]](s => s.Ints.tail.toArray)

  val pairSet = bookended.toVector
    .flatMap(_.Pairs.toSeq)
    .groupBy(identity)
    .mapValues(_.size)

  val missing = combinations.toSet.diff(pairSet.keys.toSet)

  val completePairs = (pairSet ++ missing.map(_ -> 0)).toSeq.map {
    case (k, v) => if (smooth) (k, v + 1) else (k, v)
  }

  println("Check raw data processing")
  bookended.take(5).toVector.ptbln

  println("Check pairSet")
  completePairs.take(10).toVector.ptbl

  println("plot heatmap")
  heatmap(completePairs.seq) // (using PlotTargets.doNothing)

  val grouped = completePairs.groupBy(d => d._1.head)
  val normalised = grouped.mapValues { v =>
    // println(v.sortBy(_._1.last))
    val row = v.seq.sortBy(_._1.last).map(_._2.toDouble)
    new EnumeratedIntegerDistribution(
      chars.zipWithIndex.map(_._2).toArray,
      row.toArray
    )
  }.toMap

  def generate(
      inChars: Seq[Char],
      charDist: Map[
        Char,
        EnumeratedIntegerDistribution
      ]
  ) =
    Iterator.unfold[Char, Char]('.') { c =>
      // println(s"generate $c")
      val nextChar = charDist.get(c) match
        case Some(d) =>
          val next = d.sample()
          // println(next)
          inChars(next)
        case None => '.'

      // println(nextChar)
      nextChar match
        case '.' => None
        case newChar =>
          Some(newChar -> newChar)
    }

  // println("normalised")
  // pprintln(normalised)
  // val getFirst: EnumeratedIntegerDistribution = normalised('.')

  println("Rubbish name generator")
  for (i <- 0 to 5) {
    println(generate(chars, normalised).mkString(""))
  }

  val raw = grouped.toArray.sortBy(_._1).map { case (k, v) =>
    val row = v.seq.sortBy(_._1.last).map(_._2).toArray.map(_.toDouble)
    row / row.sum
  }

  val mat = Matrix.fromRowsArray(raw)

  val probs = for ((s, idx) <- completePairs) yield {
    val l1 = s.head
    val l2 = s.last
    Math.log(normalised(l1).probability(charsIdx((l2))))
  }

  def rawVals: Iterator[String] = bookended.column["Pairs"].flatten

  def logLikelihood(strings: Iterator[String]) =
    strings.foldLeft((0.0, 0.0, 0)) { case ((sum, avg, count), s) =>
      val l1 = s.head
      val l2 = s.last
      val prob = normalised(l1).probability(charsIdx(l2))

      // println(s"$s $prob ${Math.log(prob)} ")

      val logProb = Math.log(prob)
      val newSum = sum + logProb
      val newCount = count + 1
      (newSum, (newSum / newCount), newCount)

    }

  println("-ve likelihood")
  println(logLikelihood(rawVals))

  println("check likehood of word")
  val checkWord = "simon"
  println(s"scheckword : $checkWord " + logLikelihood(".simon.".sliding(2)))

  println(
    "---- This is the bayesian pure scala bigram model, apparently, it's not great! -----"
  )
