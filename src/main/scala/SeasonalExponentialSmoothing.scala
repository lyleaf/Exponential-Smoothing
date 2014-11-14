package com.amadeus.ti.forecast.ExponentialSmoothing

import scala.math._
import scala.io.Source
import java.io._
import breeze.linalg.{DenseMatrix => BDM, DenseVector => BDV, argmax , sum => brzSum, max}



class SeasonalExponentialSmoothingModel (
    val number: Int, //The number of y that has been evaluated.
    val l_array: Array[Double],
    val b_array: Array[Double],
    val s_array: Array[Double],
    val best_index: Int,
    val MSE_vector: Array[Double],
    val m      : Int //The seasonal period, for monthly dat, m = 12, for quaterly data, m = 4.
  ) {

  def this(    //The constructor from the very beginning. With only 3 params : the number of points, initial l and inital b given. 
    l: Double,
    b: Double,
    s: Array[Double],
    m: Int
    ){
    this(0,Array.fill(1331)(l),Array.fill(1331)(b),Array.fill(1331)(s).flatten,0,Array.fill(1331)(0.0),m)
  }
  // c : calibration   n:number of parameters
  private def gridGenerator(c: Int, n: Int) = for( i <- 0 to n-1) yield List.tabulate(pow((c+1),n).toInt)(x => ((x%(pow((c+1),n-i).toInt))/(pow((c+1),n-i-1).toInt)).toDouble/c)    

  
  val IndexedSeq(alpha,beta,gama) = gridGenerator(10,3)      //Grid Search, creating for alpha and beta.
  val brzAlpha = new BDV[Double](alpha.toArray)
  val brzBeta  = new BDV[Double](beta.toArray)
  val brzGama  = new BDV[Double](gama.toArray)

  private val numOfIndex = alpha.length
  
  val brzL = new BDV[Double](l_array)
  val brzB = new BDV[Double](b_array)
  val brzMSE = new BDV[Double](MSE_vector)
  val brzS = new BDM(m,numOfIndex,s_array)
  val MSE      = brzMSE(best_index)

  def bestParams() = (brzAlpha(best_index),brzBeta(best_index),brzGama(best_index))//Get the best alpha and beta values as a tuple.
  def predict(predictionLength: Int = 12) = List.tabulate(predictionLength)(x => brzB * x.toDouble + brzB + brzL + brzS(x%m,::).t)
  def bestPrediction(predictionLength: Int = 12) = predict(predictionLength).map(_(best_index))

  def evaluate(y: Double) = {
    val new_brzL = (brzAlpha :* (-brzS(0,::).t + y)) + ((-brzAlpha + 1.0):*(brzL + brzB))
    val new_brzS = new BDM[Double](m,numOfIndex)
    new_brzS(-1,::) := ((brzGama :* (-brzL - brzB + y)) + ((-brzGama + 1.0):*(brzS(0,::).t))).t
    new_brzS(0 to -2,::) := brzS(1 to -1,::) 
    val new_brzB = (brzBeta :* (new_brzL - brzL)) + ((-brzBeta + 1.0):*brzB)
    val y_predict_1 = predict(1)(0) //Step one forecast. Type: breeze.linalg.DenseVector[Double]
    val error_1 =  y_predict_1 - y//Setp one error. Type: breeze.linalg.DenseVector[Int]
    val new_brzMSE = ( ((brzMSE :* brzMSE * number.toDouble) + (error_1 :* error_1)) * (1./(number+1))  ).map(sqrt(_)) //new MSE, Type: breeze.linalg.DenseVector[Double]
    val new_best_index = argmax(-new_brzMSE)
    new SeasonalExponentialSmoothingModel(number+1,new_brzL.toArray,new_brzB.toArray,new_brzS.data,new_best_index,new_brzMSE.toArray,m)
  }  
}



class SeasonalExponentialSmoothing {
  /**
   * Run the algorithm .
  */ 

  def initialize(y: List[Double], m: Int) = {
    val y_init = y.take(m)
    val l_0 = y_init.sum/y_init.length
    val b_0 = (y.take(2*m).drop(m).sum - y_init.sum)/m/m
    val s_0 = y_init.map(_ - l_0).toArray//(10.7,-9.5,-2.6,1.4)
    (l_0,b_0,s_0)
  }


  def run(y: List[Double],m:Int) = { 
    val number = y.length
    val (l,b,s) = initialize(y,m)
    val Model = new SeasonalExponentialSmoothingModel(l,b,s,m) //The initialization model...    
    y.foldLeft(Model)((b,a) => b.evaluate(a))
  }
}

object SeasonalExponentialSmoothing {
 
  def train(input: List[Double], m: Int): SeasonalExponentialSmoothingModel = { 
    new SeasonalExponentialSmoothing().run(input,m) //Input the the data to be forecasted and m is the period.
  }
}

/*
Comments: 
Actually, I think if I use var value instead of creating a new object would be faster because we don't need to generate the large array of 
alpha, beta, gama every time.
*/