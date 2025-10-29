
import org.apache.commons.math3.distribution.EnumeratedIntegerDistribution
import viz.LowPriorityPlotTarget
import os.Path
import vecxt.all.*


/**
 * Generates a heatmap visualization from bigram data.
 *
 * This function takes a sequence of tuples containing bigram strings and their associated
 * integer values, and renders them as an interactive heatmap.
 *
 * Here, the heatmap is essentially normalised by the highest counts.
 *
 * @param data A sequence of tuples where each tuple contains:
 *             - String: a two-character bigram (e.g., "ab", "cd")
 *             - Int: the frequency or count value for that bigram
 * @param implicitLowPriorityPlotTarget Required implicit parameter for plot rendering
 *
 * @return A plotted visualization object that renders the heatmap with the following mapping:
 *         - X-axis: first character of the bigram
 *         - Y-axis: second character of the bigram
 *         - Color intensity: the associated integer value
 *
 * @note Requires a Vega-Lite JSON specification file located at
 *       `/Users/simon/Code/makemore/data/HeatmapBigram.vg.json`
 *
 * @example
 * {{{
 * val bigramData = Seq(("ab", 42), ("cd", 15), ("ef", 28))
 * heatmap(bigramData)
 * }}}
 */

def heatmap[N : Numeric](data: collection.Seq[(String, N)], title: String)(using LowPriorityPlotTarget): Unit | Path =
  import viz.Plottable.*
  import viz.vegaFlavour
  import scala.math.Numeric.Implicits.infixNumericOps
  val spec =  os.Path("/Users/simon/Code/makemore/data/HeatmapBigram.vg.json")
  spec.plot(
    Seq(
      (spec: ujson.Value) =>
        spec("data")("values") = data.map((a, b) =>
          ujson.Obj(
            "first" -> a.charAt(0).toString(),
            "second" -> a.charAt(1).toString(),
            "value" -> b.toDouble
          )
        ),
      viz.Utils.fillDiv,
      spec => spec("title") = title
    )
  )

  /**
   * Generates a heatmap visualization of character transition probabilities.
   *
   * This function computes the probability of transitioning from each character in the input data
   * to all other characters in the character index, then creates a heatmap representation of these
   * transition frequencies.
   *
   * @param data A map where each character is associated with an EnumeratedIntegerDistribution
   *             representing the probability distribution of subsequent characters.
   * @param charsIdx A map that associates each character with a unique integer index.
   * @param int2Char A reverse map that associates integer indices back to their corresponding characters.
   * @param Using an implicit LowPriorityPlotTarget for rendering the heatmap.
   * @return Unit or a Path, depending on whether the heatmap is successfully generated and saved.
   */

def heatmap(data: Map[Char, EnumeratedIntegerDistribution], charsIdx: Map[Char, Int], int2Char: Map[Int, Char], title: String)(using LowPriorityPlotTarget): Unit | Path =
  val counts = for {
    (c1, dist) <- data.toSeq
    c2Idx <- charsIdx.values
    c2 = int2Char(c2Idx)
    count = dist.probability(c2Idx)
  } yield (s"$c1$c2", (count * 350.0).toInt)

  heatmap(counts, title)


def heatmap(matrix: Matrix[Double], chars: Array[Char], charsIndex: Array[Int], title: String)(using LowPriorityPlotTarget): Unit | Path =
  import vecxt.BoundsCheck.DoBoundsCheck.yes
  require(chars.length == charsIndex.length, "chars and charsIndex must have the same length")

  val counts = for {
    c <- 0 until matrix.cols
  } yield
    val dist = matrix.row(c)
    EnumeratedIntegerDistribution(charsIndex, dist)

  val dists = chars.zip(counts).toMap
  val charsMap = chars.zip(charsIndex).toMap
  val i2c = charsIndex.zip(chars).toMap

  heatmap(dists, charsMap, i2c, title)