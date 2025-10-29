import io.github.quafadas.table.*
import viz.PlotTargets.desktopBrowser
import org.apache.commons.math3.distribution.EnumeratedIntegerDistribution
import scala.io.Source
import vecxt.BoundsCheck.DoBoundsCheck.yes
import io.github.quafadas.inspireRAD.DetailShow
/**
 * 1. val trainData = bookended.flatMap( [find in makemore_neural_final.scala]
 *
 *
 */

@main def makemore_neural_live: Unit =
  import io.github.quafadas.inspireRAD.LiteShow.given

  val generateWeights = true
  val normalDist = new org.apache.commons.math3.distribution.NormalDistribution()
  val smooth = true

  val chars = '.' +: ('a' to 'z').toVector
  val charsIndex = (0 to 26).toVector
  val charsMap = chars.zip(charsIndex).toMap
  val i2c = charsIndex.zip(chars).toMap


  println("Characters: " + chars)
  println("Character to Index map: " + charsMap)
  println("Index to Character map: " + i2c)
