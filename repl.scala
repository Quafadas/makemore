import vecxt.all.*
import vecxt.BoundsCheck.DoBoundsCheck.yes
import io.github.quafadas.table.*

val chars = ('a' to 'c').toVector
val nums = (0 to 2).toVector
val mp = chars.zip(nums).toMap

val data = Vector(
  (first = 'a', last = 'b'),
  (first = 'b', last = 'c'),
  (first = 'c', last = 'a'),
  (first = 'b', last = 'b'),
)

val data1h = data.addColumn["onehot", String](row => onehot(row.first, mp).mkString("[", ",", "]")  )
val targets = data1h.addColumn["target", Int](row => mp(row.last))


val trainData: Matrix[Double] = Matrix.fromRowsArray(
  data.addColumn["onehot", Array[Double]](row => onehot(row.first, mp)).column["onehot"].toArray
)
// val targeted = (0 until data.length).toArray.zip(targets.column["target"])

val weights = Matrix.fromRows[Double](
    Array(1.0, 1, 7),
    Array(3.0, 3, 3),
    Array(5.0, 1, 4),
)

val findWeights =  trainData @@ weights
val probabilities = findWeights.mapRows(r => r / r.sum)
val probSelected = probabilities((0 until data.length).toArray.zip(targets.column["target"]))
val arr = probSelected.mapRowsToScalar(_.sum).raw
val likelihood = arr.log.mean * -1.0


val grad = Matrix.fromRows[Double](
    Array(0.1, 0.2, 0.3),
    Array(0.4, 0.5, 0.6),
    Array(0.7, 0.8, 0.9),
)

val learningRate = 0.5
val updateWeights = grad * learningRate
val stochasticGradientDescent = weights - updateWeights