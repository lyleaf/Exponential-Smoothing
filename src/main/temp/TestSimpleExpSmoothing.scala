import com.amadeus.ti.forecast.ExponentialSmoothing.SimpleExponentialSmoothing

object TestSimpleExpSmoothing {
  def main(args: Array[String]) {
  	val y = List( 446.6565, 454.4733, 455.6630, 423.6322 ,456.2713, 440.5881, 425.3325, 485.1494, 506.0482 ,526.7920 ,514.2689, 494.2110)
    val model = SimpleExponentialSmoothing.train(y)
    val predictions = model.predict(14)
    println(predictions)
  }
}



