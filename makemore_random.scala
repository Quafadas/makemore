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
@main def makemore_random: Unit =

  val chars = '.' +: ('a' to 'z').toVector
  val charsIndex = (0 to 26).toVector
  val charsMap = chars.zip(charsIndex).toMap
  val i2c = charsIndex.zip(chars).toMap


  val combinations = for {
    a <- chars
    b <- chars
  } yield s"$a$b"

  val data = CSV.resource("names.txt")


  val normalised = charsMap.mapValues { v =>
    // A "flat" prior - every character equally likely
    new EnumeratedIntegerDistribution(
      charsIndex.toArray,
      Array.fill(charsIndex.length)(1.0)
    )
  }.toMap

  heatmap(
    combinations.map(c => c -> 1).toSeq
  )

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
        case newChar: Char =>
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

  val dataN = CSV.resource("names.txt")
  println("Check Data")

  val logLikelihoodData = LazyList.from(dataN
    .addColumn["bookend", String]( s => s".${s.name}.")
    .addColumn["loglikelihood", Double](s => logLikelihood(s.bookend, normalised, charsMap))
    .addColumn["probability", Double](s => Math.exp(s.loglikelihood))
  )

  // Calculate basic statistics
  val avgLogLikelihood = logLikelihoodData.column["loglikelihood"].sum / logLikelihoodData.length
  val minLogLikelihood = logLikelihoodData.column["loglikelihood"].min
  val maxLogLikelihood = logLikelihoodData.column["loglikelihood"].max

  println(s"Model assessment on ${logLikelihoodData.length} names:")
  println(s"Average log-likelihood: $avgLogLikelihood")
  println(s"Min log-likelihood: $minLogLikelihood")
  println(s"Max log-likelihood: $maxLogLikelihood")

