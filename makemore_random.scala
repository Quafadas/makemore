import io.github.quafadas.table.*
import viz.PlotTargets.desktopBrowser
import org.apache.commons.math3.distribution.EnumeratedIntegerDistribution

/**
 * Live sketch:
  1. Generate chars, chars index and a bimap
  3. val normalised: Map[Char, EnumeratedIntegerDistribution] =  [find in makemore_neural_final.scala]
  4. heatmap( [find in makemore_neural_final.scala]
  5. def generator = [find in makemore_neural_final.scala]
  6. println("Rubbish name generator") [Generate names]
  7. val someNames = Seq(...) [NAMES TO CHECK]
  8. checkWords(someNames, normalised, charsMap).ptbln
 */
// scala-cli run . --main-class makemore_random_live -w
@main def makemore_random_live: Unit =

  val chars = '.' +: ('a' to 'z').toVector
  val charsIndex = (0 to 26).toVector
  val charsMap = chars.zip(charsIndex).toMap
  val i2c = charsIndex.zip(chars).toMap

  println("Characters: " + chars)
  println("Character to Index map: " + charsMap)
  println("Index to Character map: " + i2c)