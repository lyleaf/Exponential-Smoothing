import com.amadeus.ti.forecast.ExponentialSmoothing.SeasonalExponentialSmoothing

object TestSeasonalExpSmoothing {
  def main(args: Array[String]) {
    //val y = List( 41.72746, 24.04185)
    val y = List( 41.72746, 24.04185, 32.32810, 37.32871,
                 46.21315, 29.34633, 36.48291, 42.97772,
                 48.90152, 31.18022, 37.71788, 40.42021,
                 51.20686, 31.88723, 40.97826, 43.77249,
                 55.55857, 33.85092, 42.07638, 45.64229,
                 59.76678, 35.19188, 44.31974, 47.91374)
    val model = SeasonalExponentialSmoothing.train(y,4)
    val predictions = model.bestPrediction(1)
    println (model.bestParams)
    println (model.MSE)
    println(predictions)
  }
}
