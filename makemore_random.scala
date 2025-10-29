import io.github.quafadas.table.*
import viz.PlotTargets.desktopBrowser
import org.apache.commons.math3.distribution.EnumeratedIntegerDistribution
import scala.io.Source
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
@main def makemore_random_live: Unit =

  val chars = '.' +: ('a' to 'z').toVector
  val charsIndex = (0 to 26).toVector
  val charsMap = chars.zip(charsIndex).toMap
  val i2c = charsIndex.zip(chars).toMap


  println("Characters: " + chars)
  println("Character to Index map: " + charsMap)
  println("Index to Character map: " + i2c)
