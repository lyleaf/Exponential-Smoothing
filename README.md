Exponential-Smoothing in Scala
=====================

The Holt-Winters Method is a very popular and effective forecast method for forecasting time series, but its result for the forecast varies according to how we initialize the method and the way we use to optimize the parameters. As we are using scala to analyze data on Spark's HDFS system distributedly, and as you may have known that before, we use a package "RinScala" to use R to do the forecast in scala. But this requires that every distributed machine in the cluster are installed with R, which is not very efficient. And I couldn't find any good implementation for this method in scala now, so I went ahead and implemented one in scala. The main idea is to use what Hyndman's Exponential Smoothing method, which you can find in the website. https://www.otexts.org/fpp/7 What I have done basically include 3 parts.
 Simple exponential smoothing. 
Trend additive exponential smoothing. 
Seasonal-Trend additive exponential smoothing. 

For now, I haven't implemented the multiplicative methods yet, if the seasonal variance increases as the data increases, multiplicative method is better. I'll do it if I have the time. Now I'll introduce some important points that will affect the forecast results, and they are: method initialisation, parameters optimisation and prediction intervals.
 
