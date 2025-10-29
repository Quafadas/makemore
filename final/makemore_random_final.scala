import io.github.quafadas.table.*
import viz.PlotTargets.desktopBrowser
import org.apache.commons.math3.distribution.EnumeratedIntegerDistribution
/**
 * Strategy:
  1. Generate chars, chars index and a bimap
  2. Combinations of all bigrams
  3. Assume a unifiorm distribution
  4. Count pairs
  5. plot heatmap
  6. group by first char. Then Map the complete group into a EnumeratedIntegerDistribution
 *
 */
@main def makemore_random_final: Unit =

  val chars = '.' +: ('a' to 'z').toVector
  val charsIndex = (0 to 26).toVector
  val charsMap = chars.zip(charsIndex).toMap
  val i2c = charsIndex.zip(chars).toMap

  println("Characters: " + chars)
  println("Character to Index map: " + charsMap)
  println("Index to Character map: " + i2c)



  val normalised: Map[Char, EnumeratedIntegerDistribution] = charsMap.mapValues { v =>
    // A "flat" prior - every character equally likely
    new EnumeratedIntegerDistribution(
      charsIndex.toArray,
      Array.fill(charsIndex.length)(1.0)
    )
  }.toMap

  heatmap(normalised, charsMap, i2c, "Uniform Weights (i.e. Random selection)")

  def generator(
      charDist: Map[
        Char,
        EnumeratedIntegerDistribution
      ]
  ): Iterator[Char] =
    Iterator.unfold[Char, Char]('.') { c =>
      val nextCharIdx = charDist(c)
      val nextChar = nextCharIdx.sample()
      val selected = i2c(nextChar)
      selected match
        case '.' => None
        case newChar: Char =>
          Some((newChar, newChar))
    }



  println("Rubbish name generator")
  for (i <- 0 to 5) {
    println(generator(normalised).mkString(""))
  }


  val someNames = Seq("simon", "isolde", "arlo", "axel", "rrb", "zzzzzzzzz", "christoph")

  checkWords(someNames, normalised, charsMap).ptbln


  // println("Check Data")

  // val data = CSV.absolutePath("/Users/simon/Code/makemore/data/names.txt")

  // val logLikelihoodData = LazyList.from(data
  //   .addColumn["bookend", String]( s => s".${s.name}.")
  //   .addColumn["loglikelihood", (Double, Double)](s => logLikelihood(s.bookend, normalised, charsMap))
  //   .addColumn["probability", Double](s => Math.exp(s.loglikelihood._2))
  // )


  // // Calculate basic statistics
  // val avgLogLikelihood = logLikelihoodData.column["loglikelihood"].sum / logLikelihoodData.length
  // val minLogLikelihood = logLikelihoodData.column["loglikelihood"].min
  // val maxLogLikelihood = logLikelihoodData.column["loglikelihood"].max

  // println(s"Model assessment on ${logLikelihoodData.length} names:")
  // println(s"Average log-likelihood: $avgLogLikelihood")
  // println(s"Min log-likelihood: $minLogLikelihood")
  // println(s"Max log-likelihood: $maxLogLikelihood")

