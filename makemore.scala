//> using scala 3.6.2

//> using dep io.github.quafadas::scautable:0.0.17-13-17621d-DIRTYb42c0f89
//> using dep io.github.quafadas::vecxt::0.0.24
//> using dep io.github.quafadas::dedav4s:0.9.2-1-05a8a88-20250130T221142Z-SNAPSHOT
//> using dep org.apache.commons:commons-math3:3.6.1

//> using options "-experimental" "-language:experimental.namedTuples"

//> using javaOpt "--add-modules=jdk.incubator.vector"

//> using resourceDir "./data"

import io.github.quafadas.table.*

import viz.NamedTupleReadWriter.given
import viz.PlotNt.plot
import org.apache.commons.math3.distribution.EnumeratedIntegerDistribution

// import scala.NamedTuple
// import scala.NamedTuple.*
import vecxt.all.*
import viz.NamedTupleReadWriter.given
import viz.PlotTargets.doNothing
import viz.FromResource
import viz.PlotTarget
// import vecxt.*
import viz.PlotTargets
import pprint.*
import scala.collection.View
import scala.collection.immutable.HashMap
import vecxt.BoundsCheck.DoBoundsCheck.yes

type JsonMod = ujson.Value => Unit
case class HeatmapBigram(override val mods: Seq[JsonMod] = List())(using
    PlotTarget
) extends FromResource:
  override lazy val path = "HeatmapBigram.vg.json"

def heatmap(data: Seq[(String, Int)])(using PlotTarget) =
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

@main def makemore(): Unit =

  val normalDist =
    new org.apache.commons.math3.distribution.NormalDistribution()

  val smooth = true

  val chars = '.' +: ('a' to 'z').toVector
  val charsIdx = chars.zipWithIndex.toMap

  def data = CSV.resource("names.txt")
  def bookended = data
    .addColumn["Bookend", String](s => s".${s.name}.")
    .addColumn["Pairs", Seq[String]](s => s.Bookend.sliding(2).toSeq)
    .addColumn["Ints", Seq[Int]](s => s.Bookend.map(charsIdx))

  val combinations = for {
    a <- chars
    b <- chars
  } yield s"$a$b"

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
  heatmap(completePairs) // (using PlotTargets.doNothing)

  val grouped = completePairs.groupBy(d => d._1.head)
  val normalised: Map[Char, EnumeratedIntegerDistribution] = grouped.mapValues {
    v =>
      // println(v.sortBy(_._1.last))
      val row = v.sortBy(_._1.last).map(_._2.toDouble).toArray
      new EnumeratedIntegerDistribution(
        chars.zipWithIndex.map(_._2).toArray,
        row
      )
  }.toMap

  def generate(
      inChars: Vector[Char],
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
    val row = v.sortBy(_._1.last).map(_._2).toArray.map(_.toDouble)
    row / row.sum
  }

  val mat = Matrix.fromRows(raw)

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
  println("Change lange to neural network")

  val randn = Matrix(Array.fill(27 * 27)(normalDist.sample()), (27, 27))

  println(randn.printMat)

  // mat(::, *)
