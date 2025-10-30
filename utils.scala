import io.github.quafadas.table.*
import org.apache.commons.math3.distribution.EnumeratedIntegerDistribution
import vecxt.all.*
import scala.reflect.ClassTag

extension (m: Matrix[Double])
  inline def pt = println(m.printMat)

/**
 * Calculates the average log-likelihood of a string given a character transition model.
 *
 * This function computes the log probability of each character transition in the string,
 * where each transition is modeled as a probability distribution conditioned on the previous character.
 * The result is normalized by the number of transitions (string length - 1).
 *
 * @param string the input string to evaluate
 * @param charmap a map from each character to its probability distribution over the next character
 * @param charsMap a map from each character to its integer index for use in the probability distribution
 * @return the average log-likelihood of the string, normalized by the number of character transitions
 */

def logLikelihood(string: String, charmap: Map[Char, EnumeratedIntegerDistribution], charsMap: Map[Char, Int]) =
  val sum = string.sliding(2).foldLeft(0.0) { (sum, s) =>
    val l1 = s.head
    val l2 = s.last
    val prob = charmap(l1).probability(charsMap(l2))
    val logProb = Math.log(prob)
    sum + logProb
  }
  (totalLikelihood = sum, prob = Math.exp(sum), avg_prbability = Math.exp(sum / (string.length - 1)))

/**
 * Calculates the probability of a given string based on character-level transitions.
 *
 * This function computes the joint probability of a string by treating it as a sequence
 * of character pairs (bigrams) and multiplying the conditional probabilities of each
 * subsequent character given the previous one.
 *
 * @param string the input string for which to calculate probability
 * @param charmap a map from each character to its corresponding probability distribution
 *                over possible next characters
 * @param charsMap a map from characters to their integer indices used in the distributions
 * @return the probability of the string as a Double value, calculated as the product
 *         of all bigram conditional probabilities (using log-space arithmetic for
 *         numerical stability)
 *
 * @note uses logarithmic space to avoid numerical underflow when multiplying small probabilities,
 *       then converts back to probability space via exponentiation
 */
def probabilityWord(string: String, charmap: Map[Char, EnumeratedIntegerDistribution], charsMap: Map[Char, Int]): Double =
  val logProb = string.sliding(2).foldLeft(0.0) { (sum, s) =>
    val l1 = s.head
    val l2 = s.last
    val prob = charmap(l1).probability(charsMap(l2))
    sum + Math.log(prob)
  }
  Math.exp(logProb)

/**
 * Checks a word and prints its probability metrics.
 *
 * This function evaluates a given word by computing its probability, log-likelihood,
 * and normalized probability based on the provided character distributions and mappings.
 * Results are printed to the console with the format:
 * "checkword : [word] Probability: [prob] LogLikelihood: [loglik] prb (normalised): [normalized]"
 *
 * @param str the word to check
 * @param charmap a map of characters to their enumerated integer distributions
 * @param charsMap a map of characters to their integer encodings
 * @return Unit (prints results to console as a side effect)
 */
def checkWord(str: String, charmap: Map[Char, EnumeratedIntegerDistribution], charsMap: Map[Char, Int]) : Unit =
  val bookend = s".$str."
  println(s"checkword : $str ")
  Seq(logLikelihood(bookend, charmap, charsMap)).ptbln

def checkWords(strs : Seq[String], charmap: Map[Char, EnumeratedIntegerDistribution], charsMap: Map[Char, Int]) =
  for (str <- strs)  yield {
    (word = str) ++ logLikelihood(s".$str.", charmap, charsMap)
  }



def graphDebug(s: String) =
  os.write.over(os.Path("/Users/simon/Code/makemore") / "graph.dot", s)

def saveMatrixToCSV(matrix: Matrix[Double], filePath: String): Unit =
  val lines = for (i <- 0 until matrix.rows) yield matrix.row(i).mkString(",")
  os.write.over(os.Path(filePath, os.pwd), lines.mkString("\n"))

def loadMatrixFromCSV(filePath: String)(using
    ClassTag[Double]
): Matrix[Double] =
  import vecxt.BoundsCheck.DoBoundsCheck.yes
  val lines = os.read.lines(os.Path(filePath, os.pwd))
  val data = lines.map(_.split(",").map(_.toDouble)).toArray
  Matrix.fromRowsArray(data)