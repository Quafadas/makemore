import io.github.quafadas.table.*
import viz.PlotTargets.desktopBrowser
import org.apache.commons.math3.distribution.EnumeratedIntegerDistribution



/**
 * Strategy:
  1. Generate chars, chars index and a bimap
  2. val combinations = [Combinations of all bigrams]
  3. val data = CSV.absolutePath( [LOAD DATA]
  4. val bookended = LazyList.from( [BOOKEND AND EXTRACT PAIRS]
  5. val pairSet = bookended.flatMap( [COUNT PAIRS]
  6. val missing = combinations.toSet.diff( [FIND MISSING COMBINATIONS]
  7. val completePairs = (pairSet ++ missing.map( [MAKE COMPLETE SET
  8. val grouped = completePairs.groupBy( [GROUP BY FIRST CHAR]
  9. val normalised = grouped.mapValues( [MAKE DISTRIBUTIONS]
  10. heatmap(completePairs.seq, [PLOT RAW COUNTS])
  11. heatmap(normalised, charsMap, i2c, [PLOT PROBABILITY WEIGHTED])
  12. def generator( [GENERATE NAMES]
  13. println("Moderately less rubbish name generator") [GENERATE NAMES]
  14. val someNames = Seq(...) [NAMES TO CHECK]
  15. checkWords(someNames, normalised, charsMap).ptbln

 *
 */
// scala-cli run . --main-class makemore_bayesian_live -w
@main def makemore_bayesian_final: Unit =

  val smooth = true

  val chars = '.' +: ('a' to 'z').toVector
  val charsIndex = (0 to 26).toVector
  val charsMap = chars.zip(charsIndex).toMap
  val i2c = charsIndex.zip(chars).toMap


  val combinations = for {
    a <- chars
    b <- chars
  } yield s"$a$b"

//https://github.com/VirtusLab/scala-cli/issues/3907
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
      .addColumn["Ints", Seq[Int]](s => s.Bookend.map(charsMap))
  )

  println("Examples")
  bookended.take(5).ptbln

  val pairSet: Map[String, Int] = bookended
    .flatMap(_.Pairs)
    .groupMapReduce(identity)(_ => 1)(_ + _)

  val missing: Set[String] = combinations.toSet.diff(pairSet.keys.toSet)

  println("missing combinations")
  println(missing)

  val completePairs: Seq[(String, Int)] = pairSet.toSeq ++ missing.map(char => (char, if(smooth) 1 else 0))

  val grouped: Map[Char, Seq[(String, Int)]] = completePairs.groupBy(d => d._1.head)

  val normalised: Map[Char, EnumeratedIntegerDistribution] = grouped.mapValues { v =>
    val sortedByChar = v.sortBy(_._1.last)
    val col = sortedByChar.map(_._2.toDouble)
    new EnumeratedIntegerDistribution(
      charsIndex.toArray,
      col.toArray
    )
  }.toMap

  println("plot heatmap")
  heatmap(completePairs.seq, "Character bigram counts (not probability weighted)")
  heatmap(normalised, charsMap, i2c, "Character bigram (probability weighted)")

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


  println("Moderately less rubbish name generator")
  for (i <- 0 to 5) {
    println(generator(normalised).mkString(""))
  }

  val someNames = Seq("simon", "isolde", "arlo", "axel", "zzz", "zzzzzzzzz", "christoph")

  checkWords(someNames, normalised, charsMap).ptbln
