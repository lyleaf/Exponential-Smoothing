package com.amadeus.ti.forecast.ExponentialSmoothing

import scala.math._
import scala.io.Source
import java.io._
import breeze.linalg.{DenseMatrix => BDM, DenseVector => BDV, argmax , sum => brzSum, max}

class SimpleExponentialSmoothingModel (val forecast_values: Double){
  
  private val forecast_value = forecast_values

  def predict(predictionLength: Int = 12): List[Double] = {
    val predictValues = List.fill(predictionLength)(forecast_value)
    return predictValues
    }
  }

class SimpleExponentialSmoothing {
  /**
   * Run the algorithm with the configured parameters on an input RDD of LabeledPoint entries.
   *
   * 
   */
  def run(y: List[Double]) = {
    val alpha = BDV.tabulate(101)(_/100.)
    val y_vector = new BDV[Double](y.toArray)

    def l(n: Int): BDV[Double] = {
      if (n==0){
        return BDV.fill(alpha.length){y(0)}//returns initial value l(0) = y(1)
      } else {
        return (alpha :* y(n-1)) + ((-alpha+1.0):*l(n-1))
      }
    }

    val l_vector = BDV.tabulate(y_vector.length)(x => l(x+1))//l(1) --- l(30)
    val e_vector = BDV.tabulate(y_vector.length-1){i => l_vector(i) - y_vector(i+1)}.map(x => x:*x).reduce(_ + _)
    val MSE_vector=  (e_vector(0) / y_vector.length.toDouble).map(sqrt(_))
    val best_index = argmax(-MSE_vector)
    println("Best Alpha:" + (alpha(best_index)).toString)
    val forecast_vector = l_vector.map(x => x(best_index))
    val forecast_values = forecast_vector(-1)

    new SimpleExponentialSmoothingModel(forecast_values)
  }
}

object SimpleExponentialSmoothing {
 
  def train(input: List[Double]): SimpleExponentialSmoothingModel = {
    new SimpleExponentialSmoothing().run(input)
  }
}
  /*
val y = List( 446.6565, 454.4733, 455.6630, 423.6322 ,456.2713, 440.5881, 425.3325, 485.1494, 506.0482 ,526.7920 ,514.2689, 494.2110)
val y_vector = new BDV[Double](y.toArray)
val m = BDV[Double](446.7,448.2,449.7,444.5,446.8,445.6,441.5,450.3,461.4,474.5,482.5,484.8)
val e___ = m(0 to -2) - y_vector(1 to -1)
val e__ = e___.map(x => x*x).reduce(_+_)
val MSE = sqrt(e__(0)/(e___.length+1))
 val alpha = BDV.tabulate(11)(_/10.)
  //val y:List[Double] = List.tabulate(30)(_.toDouble+1)
  val y = List( 446.6565, 454.4733, 455.6630, 423.6322 ,456.2713, 440.5881, 425.3325, 485.1494, 506.0482 ,526.7920 ,514.2689, 494.2110)
  val y_vector = new BDV[Double](y.toArray)

  def l(n: Int): BDV[Double] = {
    if (n==0){
      return BDV.fill(alpha.length){y(0)}//returns initial value l(0) = y(1)
    } else {
      return (alpha :* y(n-1)) + ((-alpha+1.0):*l(n-1))
    }
  }
   val l_vector = BDV.tabulate(y_vector.length)(x => l(x+1))//l(1) --- l(30)
  val e_vector = BDV.tabulate(y_vector.length-1){i => l_vector(i) - y_vector(i+1)}.map(x => x:*x).reduce(_ + _)
  val MSE_vector = (e_vector(0) / y_vector.length.toDouble).map(sqrt(_))
  val best_index = argmax(-MSE_vector)
  val forecast_vector = l_vector.map(x => x(best_index))
  val forecast_values = forecast_vector(-1)
*/