
forecastArima <- function(returns, nome,  n, l){
  returns<-as.zoo(returns)
  fitRet <- stl(returns[,1], s.window = "period")
  plot(fitRet, main=paste("Seasonal Decomposition of",nome,"CC Returns Time Series"))
  returnsTrain <- returns[1:n]  
  length(returnsTrain)
  tail(returnsTrain)
  returnsTest <- returns[(n+1):(n+l)]  
  length(returnsTest)
  returnsTest
  data <- auto.arima(returnsTrain)
  summary(data)
  predictions <- predict(data, 10)
  new_data <- rbind(returnsTrain, predictions$pred)
  testo <- paste("ARMA forecasts for", nome, "returns")
  plot(new_data, main = testo)
  upper <- predictions$pred + predictions$se
  lower <- predictions$pred - predictions$se
  polygon(c(index(upper), rev(index(upper))), 
          c(upper, rev(lower)), col="lightblue", border=NA) 
  lines(lower, col='green')
  lines(upper, col='green')
  lines(predictions$pred,col='red')
  print(accuracy(predictions$pred, returnsTest))
}
print("------ARIMA Forcasting of AAPL------")
forecastArima(Merged_rtn_cc.xts$AAPL, "AAPL", 96, 10)
print("------ARIMA Forcasting of MSFT------")
forecastArima(Merged_rtn_cc.xts$MSFT, "MSFT", 96, 10)
print("------ARIMA Forcasting of AMZN------")
forecastArima(Merged_rtn_cc.xts$AMZN, "AMZN", 96, 10)
print("------ARIMA Forcasting of GOOG------")
forecastArima(Merged_rtn_cc.xts$GOOG, "GOOG", 96, 10)


