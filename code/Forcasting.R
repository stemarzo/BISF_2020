source("Forcasting_2.R")
forecastArima <- function(returns, nome,  n, m, l, param){
  returns<-as.zoo(returns)
  fitRet <- stl(returns[,1], s.window = "period")
  plot(fitRet, main=paste("Seasonal Decomposition of",nome,"CC Returns Time Series"))
  
  returnsTrain <- returns[1:n, drop= FALSE]  
  returnsValid <- returns[(n+1):(n+m), drop = FALSE]
  returnsTest <- returns[(length(returns)-l+1):length(returns), drop=FALSE] 
  set <- rbind(returnsTrain, returnsValid)
  data <- arima(set, param)
  predictions <- predict(data, l)
  new_data <- rbind(set, predictions$pred)
  testo <- paste("ARMA forecasts for", nome, "returns")
  plot(new_data, main = testo)
  upper <- predictions$pred + predictions$se
  lower <- predictions$pred - predictions$se
  polygon(c(index(upper), rev(index(upper))), 
          c(upper, rev(lower)), col="lightblue", border=NA) 
  lines(lower, col='green')
  lines(upper, col='green')
  lines(predictions$pred,col='red')
  #lines(returnsTest)
  print(accuracy(predictions$pred, returnsTest)[2])
}
print("------ARIMA Forcasting of AAPL------")
forecastArima(Merged_rtn_cc.xts$AAPL, "AAPL", n, m, l, AAPL_Arima[2,])
print("------ARIMA Forcasting of MSFT------")
forecastArima(Merged_rtn_cc.xts$MSFT, "MSFT", n, m, l, MSFT_Arima[2,])
print("------ARIMA Forcasting of AMZN------")
forecastArima(Merged_rtn_cc.xts$AMZN, "AMZN", n, m, l, AMZN_Arima[2,])
print("------ARIMA Forcasting of GOOG------")
forecastArima(Merged_rtn_cc.xts$GOOG, "GOOG", n, m, l, GOOG_Arima[2,])

