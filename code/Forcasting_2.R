ritornoAccuracy <- function(train, valid, m, parAr){
  data <- arima(train, order = parAr)
  predictions <- predict(data, m)
  return(accuracy(predictions$pred, valid)[2])
}

MigliorArima <- function(train, valid, m){
  minimo <- 100
  for(p in 0:10){
    for(q in 0:10){
      tryCatch(
        expr = {
          RMSE <- ritornoAccuracy(train, valid, m, c(p, 0, q))
        },
        error = function(e){
          RMSE <- 1000
        }
      )
      
      if(RMSE < minimo){
        minimo <- RMSE
        minP <- p
        minQ <- q
      }
    }
  }
  param <- c(minP,0,minQ)
  return(rbind(minimo, param))
}

AAPL_Arima <- MigliorArima(AAPL[1:n,drop=FALSE], AAPL[(n+1):(n+m), drop=FALSE], m)
MSFT_Arima <- MigliorArima(MSFT[1:n,drop=FALSE], MSFT[(n+1):(n+m), drop=FALSE], m)
AMZN_Arima <- MigliorArima(AMZN[1:n,drop=FALSE], AMZN[(n+1):(n+m), drop=FALSE], m)
GOOG_Arima <- MigliorArima(GOOG[1:n,drop=FALSE], GOOG[(n+1):(n+m), drop=FALSE], m)
