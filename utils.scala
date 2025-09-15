import org.apache.commons.math3.distribution.EnumeratedIntegerDistribution

def logLikelihood(string: String, charmap: Map[Char, EnumeratedIntegerDistribution], charsMap: Map[Char, Int]): Double =
  val sum = string.sliding(2).foldLeft(0.0) { (sum, s) =>
    val l1 = s.head
    val l2 = s.last
    val prob = charmap(l1).probability(charsMap(l2))
    val logProb = Math.log(prob)
    sum + logProb
  }
  sum / (string.length() - 1).toDouble



def checkWord(str: String, charmap: Map[Char, EnumeratedIntegerDistribution], charsMap: Map[Char, Int]) : Unit =
  val bookend = s".$str."
  println(s"checkword : $str " + logLikelihood(bookend, charmap, charsMap) + " prb " + Math.exp(logLikelihood(bookend, charmap, charsMap)))