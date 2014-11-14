package com.amadeus.ti.forecast.ExponentialSmoothing

import scala.math._
import scala.io.Source
import java.io._
import breeze.linalg.{DenseMatrix => BDM, DenseVector => BDV, argmax , sum => brzSum, max}

class TrendExponentialSmoothingModel (
    val number: Int, //The number of y that has been evaluated.
    val l_array: Array[Double],
    val b_array: Array[Double],
    val s_array: Array[Double],
    val best_index: Int,
    val MSE_vector: Array[Double],
    val m      : Int
  ) {

  def this(    //The constructor from the very beginning. With only 3 params : the number of points, initial l and inital b given. 
    l: Double,
    b: Double,
    s: Array[Double],
    m: Int
    ){
    this(0,Array.fill(121)(l),Array.fill(121)(b),Array.fill(121)(s).flatten,0,Array.fill(121)(0.0),m)
  }


  private val numOfIndex = 121

  val brzL = new BDV[Double](l_array)
  val brzB = new BDV[Double](b_array)
  val brzMSE = new BDV[Double](MSE_vector)
  val brzS = new BDM(m,121,large)

  val brzAlpha = BDV.tabulate(121)(x => (x/11).toDouble/10.) //Grid Search, creating for alpha and beta.
  val brzBeta  = BDV.tabulate(121)(x => (x%11).toDouble/10.)
  val brzGama  = BDV.tabulate(121)(x => (x%11).toDouble/10.)
  val MSE      = brzMSE(best_index)

  def bestParams() = (brzAlpha(best_index),brzBeta(best_index),brzGama(best_index))//Get the best alpha and beta values as a tuple.
  def predict(predictionLength: Int = 12) = List.tabulate(predictionLength)(x => brzB * x.toDouble + brzB + brzL + brzS(x%m,::).t)
  def bestPrediction(predictionLength: Int = 12) = predict(predictionLength).map(_(best_index))

  def evaluate(y: Double) = {
    val new_brzL = (brzAlpha :* (-brzS(0,::).t + y)) + ((-brzAlpha + 1.0):*(brzL + brzB))
    val new_brzS = new BDM[Double](s_array(0).length,s_array.length)
    new_brzS(-1,::) := ((brzGama :* (-brzL - brzB + y)) + ((-brzGama + 1.0):*(brzS(0,::).t))).t
    new_brzS(0 to -2,::) := brzS(1 to -1,::) 
    val new_brzB = (brzBeta :* (new_brzL - brzL)) + ((-brzBeta + 1.0):*brzB)
    val y_predict_1 = predict(1)(0) //Step one forecast. Type: breeze.linalg.DenseVector[Double]
    val error_1 =  y_predict_1 - y//Setp one error. Type: breeze.linalg.DenseVector[Int]
    val new_brzMSE = ( ((brzMSE :* brzMSE * number.toDouble) + (error_1 :* error_1)) * (1./(number+1))  ).map(sqrt(_)) //new MSE, Type: breeze.linalg.DenseVector[Double]
    val new_best_index = argmax(-new_brzMSE)
    new TrendExponentialSmoothingModel(number+1,new_brzL.toArray,new_brzB.toArray,new_brzS.data,new_best_index,new_brzMSE.toArray)
  }  
}

class TrendExponentialSmoothing {
  /**
   * Run the algorithm .
  */ 

  def initialize(y: List[Double], h: Int = 3) = {
    val l_0 = y(0)
    val b_0 = y(1) - y(0)  
    val s_0 = Array(10.7,-9.5,-2.6,1.4)
    (l_0,b_0,s_0)
  }


  def run(y: List[Double],m:Int) = { 
    val number = y.length
    val (l,b,s) = initialize(y)
    val Model = new TrendExponentialSmoothingModel(l,b,s,m) //The initialization model...    
    y.foldLeft(Model)((b,a) => b.evaluate(a))
  }
}

object TrendExponentialSmoothing {
 
  def train(input: List[Double]): TrendExponentialSmoothingModel = {
    new TrendExponentialSmoothing().run(input,m) //Input the the data to be forecasted and m is the period.
  }
}

