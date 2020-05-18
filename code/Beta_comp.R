
calcolo_beta <- function(rtn_azione, indice){
  beta <- cov(rtn_azione, indice)/var(indice)
  return(beta)
}

SP500.xts <- scarica_dati("^GSPC", start_stream, end_stream)
SP500 <- na.omit(diff(log(SP500.xts)))#CC_rtn of SP500
colnames(SP500) <- c("SP500")


AAPL_betas.xts <- NULL 
MSFT_betas.xts <- NULL
AMZN_betas.xts <- NULL
GOOG_betas.xts <- NULL
delta_t <- 12 
length_period = dim(SP500)[1]
start <- delta_t+1 

for (i in start:length_period){
  beta_AAPL <- calcolo_beta(AAPL[(i-delta_t):(i-1)], SP500[(i-delta_t):(i-1)])
  c_beta_AAPL <- as.xts(beta_AAPL, order.by = index(AAPL[(i-1)]))
  beta_MSFT <- calcolo_beta(MSFT[(i-delta_t):(i-1)], SP500[(i-delta_t):(i-1)])
  c_beta_MSFT <- as.xts(beta_MSFT, order.by = index(MSFT[(i-1)]))
  beta_AMZN <- calcolo_beta(AMZN[(i-delta_t):(i-1)], SP500[(i-delta_t):(i-1)])
  c_beta_AMZN <- as.xts(beta_AMZN, order.by = index(MSFT[(i-1)]))
  beta_GOOG <- calcolo_beta(GOOG[(i-delta_t):(i-1)], SP500[(i-delta_t):(i-1)])
  c_beta_GOOG <- as.xts(beta_GOOG, order.by = index(GOOG[(i-1)]))
  if(is.null(AAPL_betas.xts)){
    AAPL_betas.xts <- c_beta_AAPL
    MSFT_betas.xts <- c_beta_MSFT
    AMZN_betas.xts <- c_beta_AMZN
    GOOG_betas.xts <- c_beta_GOOG
  }else{
    AAPL_betas.xts <- rbind(AAPL_betas.xts,c_beta_AAPL)
    MSFT_betas.xts <- rbind(MSFT_betas.xts,c_beta_MSFT)
    AMZN_betas.xts <- rbind(AMZN_betas.xts,c_beta_AMZN)
    GOOG_betas.xts <- rbind(GOOG_betas.xts,c_beta_GOOG)
  }
}
Merged_betas <- merge(AAPL_betas.xts, MSFT_betas.xts, AMZN_betas.xts, GOOG_betas.xts) 
colnames(Merged_betas) <- assets_name

mostraGraficoBeta <- function(dati, titolo){
  data_frame<-data.frame(date = index(dati), coredata(dati))
  melt_data <- melt(data_frame, id = "date")
  ggplot(data = melt_data, aes(x = as.Date(date), y = value, colour = variable)) +
    geom_line(size = 1, show.legend = FALSE) +
    scale_x_date() +
    scale_y_continuous() +
    labs(y = "Value", x = "Data", colour = "", title = titolo)+
    theme_minimal()
}

calcolaRitornoBeta <- function(beta, risk_free, r_market){
  return(risk_free+(beta*(r_market-risk_free)))
}

