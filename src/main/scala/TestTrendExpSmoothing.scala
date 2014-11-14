import com.amadeus.ti.forecast.ExponentialSmoothing.TrendExponentialSmoothing

object TestTrendExpSmoothing {
  def main(args: Array[String]) {
  	val y = List( 17.55340, 21.86010, 23.88660, 26.92930, 26.88850, 28.83140, 30.07510, 30.95350, 30.18570, 31.57970,32.57757, 33.47740 ,39.02158, 41.38643, 41.59655)
    val model = TrendExponentialSmoothing.train(y)
    println (model.bestParams)
    println (model.MSE)
    val predictions = model.bestPrediction(12)
    println(predictions)
  }
}