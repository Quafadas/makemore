import io.github.quafadas.table.*
import viz.PlotTargets.desktopBrowser
import org.apache.commons.math3.distribution.EnumeratedIntegerDistribution



/**
 * Strategy:
  1. Generate chars, chars index and a bimap
  2. Combinations of all bigrams
  3. ingest data - add bookends, pairs and int representation
  4. Count pairs
  5. plot heatmap
  6. group by first char. Then Map the complete group into a EnumeratedIntegerDistribution

 *
 */
@main def makemore_bayesian: Unit =

  val smooth = true

  val chars = '.' +: ('a' to 'z').toVector
  val charsIndex = (0 to 26).toVector
  val charsMap = chars.zip(charsIndex).toMap
  val i2c = charsIndex.zip(chars).toMap


  val combinations = for {
    a <- chars
    b <- chars
  } yield s"$a$b"

  val data = CSV.resource("names.txt")


/** Bookended : adds "."to either end of the string Pairs : extracts the
  * series of pairs of the bookended characters characters Ints : indexes the
  * bookended characters xenc : one hot encodes the characters excluding the
  * last character
  */

  val bookended = LazyList.from(
    data
      .addColumn["Bookend", String](s => s".${s.name}.")
      .addColumn["Pairs", Seq[String]](s => s.Bookend.sliding(2).toSeq)
      .addColumn["Ints", Seq[Int]](s => s.Bookend.map(charsMap))
  )

  println("Examples")
  bookended.take(5).ptbln

  val pairSet = bookended
    .flatMap(_.Pairs)
    .groupMapReduce(identity)(_ => 1)(_ + _)

  println(pairSet.toSeq.sortBy(_._2).takeRight(10))

  val missing = combinations.toSet.diff(pairSet.keys.toSet)

  println("missing combinations")
  println(missing)

  val completePairs = (pairSet ++ missing.map(_ -> 0)).toSeq.map {
    case (k, v) => if (smooth) (k, v + 1) else (k, v)
  }
  println("plot heatmap")
  heatmap(completePairs.seq)


  val grouped = completePairs.groupBy(d => d._1.head)
  val normalised = grouped.mapValues { v =>
    val sortedByChar = v.sortBy(_._1.last)
    val row = sortedByChar.map(_._2.toDouble)
    new EnumeratedIntegerDistribution(
      charsIndex.toArray,
      row.toArray
    )
  }.toMap

  def generator(
      charDist: Map[
        Char,
        EnumeratedIntegerDistribution
      ]
  ) =
    Iterator.unfold[Char, Char]('.') { c =>
      val nextCharIdx = charDist(c)
      val nextChar = i2c(nextCharIdx.sample())
      nextChar match
        case '.' => None
        case newChar =>
          Some((newChar, newChar))
    }


  println("Rubbish name generator")
  for (i <- 0 to 5) {
    println(generator(normalised).mkString(""))
  }


  checkWord("simon", normalised, charsMap)
  checkWord("isolde", normalised, charsMap)
  checkWord("arlo", normalised, charsMap)
  checkWord("axel", normalised, charsMap)
  checkWord("zqzvzcvs", normalised, charsMap)


  // // Calculate basic statistics
  // val avgLogLikelihood = logLikelihoodData.column["loglikelihood"].sum / logLikelihoodData.length
  // val minLogLikelihood = logLikelihoodData.column["loglikelihood"].min
  // val maxLogLikelihood = logLikelihoodData.column["loglikelihood"].max

  // println(s"Model assessment on ${logLikelihoodData.length} names:")
  // println(s"Average log-likelihood: $avgLogLikelihood")
  // println(s"Min log-likelihood: $minLogLikelihood")
  // println(s"Max log-likelihood: $maxLogLikelihood")