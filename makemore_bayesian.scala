import io.github.quafadas.table.*
import viz.PlotTargets.desktopBrowser
import org.apache.commons.math3.distribution.EnumeratedIntegerDistribution
import scala.io.Source
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

 */
// scala-cli run . --main-class makemore_bayesian_live -w
@main def makemore_bayesian_live: Unit =
  // Whether to "smooth" the counts...
  val smooth = false

  val chars = '.' +: ('a' to 'z').toVector
  val charsIndex = (0 to 26).toVector
  val charsMap = chars.zip(charsIndex).toMap
  val i2c = charsIndex.zip(chars).toMap


  println("Characters: " + chars)
  println("Character to Index map: " + charsMap)
  println("Index to Character map: " + i2c)
