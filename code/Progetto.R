#------------MARZORATI STEFANO 830272-------------------------

#Set working directory to source file

rm(list=ls())
set.seed(1)

#LIBRERIE

library(tseries)
library(quantmod)
library(reshape2)
library(forecast)
library(PerformanceAnalytics)
library(ggplot2)
library(zoo)
library(shiny)

#FILE CON FUNZIONI
source("Data_Desc.R")


assets_name <- c("AAPL", "MSFT", "AMZN", "GOOG")

start_stream <- '2010-01-01'
end_stream <- '2019-10-31'

AAPL_adj_close.xts <- scarica_dati("AAPL", start_stream, end_stream)
MSFT_adj_close.xts <- scarica_dati("MSFT", start_stream, end_stream)
AMZN_adj_close.xts <- scarica_dati("AMZN", start_stream, end_stream)
GOOG_adj_close.xts <- scarica_dati("GOOG", start_stream, end_stream)

Merged_assets_price.xts <- merge(AAPL_adj_close.xts, MSFT_adj_close.xts, AMZN_adj_close.xts, GOOG_adj_close.xts)
colnames(Merged_assets_price.xts) <- assets_name
mostraGrafico(Merged_assets_price.xts,"Adjusted Close Price of Assets")

#CALCOLO DEI RITORNI
Merged_rtn_simple.xts <- na.omit(CalculateReturns(Merged_assets_price.xts, method = "simple"))
Merged_rtn_cc.xts <- na.omit(CalculateReturns(Merged_assets_price.xts, method="compound"))
mostraGrafico(Merged_rtn_simple.xts, "Simple Return of the Assets")
mostraGrafico(Merged_rtn_cc.xts, "CC Return of the Assets")

AAPL <- Merged_rtn_cc.xts$AAPL
MSFT <- Merged_rtn_cc.xts$MSFT
AMZN <- Merged_rtn_cc.xts$AMZN
GOOG <- Merged_rtn_cc.xts$GOOG

#ISTOGRAMMI DEI RITORNI
mostraIstogramma("AAPL", "Distribuzione di CC Return abount AAPL", Merged_rtn_cc.xts$AAPL,"#e22f7b", "white", c(0,6), c(-0.2,0.2))
points(density(Merged_rtn_cc.xts$AAPL), type="l", col="black")

mostraIstogramma("MSFT", "Distribuzione di CC Return abount MSFT", Merged_rtn_cc.xts$MSFT,"#2fbe81", "white", c(0,9), c(-0.2,0.2))
points(density(Merged_rtn_cc.xts$MSFT), type="l", col="black")

mostraIstogramma("AMZN", "Distribuzione di CC Return abount AMZN", Merged_rtn_cc.xts$AMZN,"#12b1ff", "white", c(0,8), c(-0.15,0.2))
points(density(Merged_rtn_cc.xts$AMZN), type="l", col="black")

mostraIstogramma("GOOG", "Distribuzione di CC Return abount GOOG", Merged_rtn_cc.xts$GOOG,"#ffcd12", "white", c(0,7), c(-0.25,0.25))
points(density(Merged_rtn_cc.xts$GOOG), type="l", col="black")


multiplePlot("AAPL", "Distribuzione di CC Return abount AAPL", Merged_rtn_cc.xts$AAPL,"#e22f7b", "white", c(0,6), c(-0.2,0.2))
multiplePlot("MSFT", "Distribuzione di CC Return abount MSFT", Merged_rtn_cc.xts$MSFT,"#2fbe81", "white", c(0,9), c(-0.2,0.2))
multiplePlot("AMZN", "Distribuzione di CC Return abount AAPL", Merged_rtn_cc.xts$AMZN,"#12b1ff", "white", c(0,8), c(-0.15,0.2))
multiplePlot("GOOG", "Distribuzione di CC Return abount GOOG", Merged_rtn_cc.xts$GOOG,"#ffcd12", "white", c(0,6), c(-0.25,0.25))

#CALCOLO ALCUNI PARAMETRI DEGLI ASSETS
Mean_AAPL <- mean(Merged_rtn_cc.xts$AAPL)
Variance_AAPL <- var(Merged_rtn_cc.xts$AAPL)[1]
SD_AAPL <- sd(Merged_rtn_cc.xts$AAPL)
SK_AAPL <- skewness(Merged_rtn_cc.xts$AAPL)
KUR_AAPL <- kurtosis(Merged_rtn_cc.xts$AAPL)
Qnorm_AAPL <- quantile(Merged_rtn_cc.xts$AAPL)

Mean_MSFT <- mean(Merged_rtn_cc.xts$MSFT)
Variance_MSFT <- var(Merged_rtn_cc.xts$MSFT)[1]
SD_MSFT <- sd(Merged_rtn_cc.xts$MSFT)
SK_MSFT <- skewness(Merged_rtn_cc.xts$MSFT)
KUR_MSFT <- kurtosis(Merged_rtn_cc.xts$MSFT)
Qnorm_MSFT <- quantile(Merged_rtn_cc.xts$MSFT)

Mean_AMZN <- mean(Merged_rtn_cc.xts$AMZN)
Variance_AMZN <- var(Merged_rtn_cc.xts$AMZN)[1]
SD_AMZN <- sd(Merged_rtn_cc.xts$AMZN)
SK_AMZN <- skewness(Merged_rtn_cc.xts$AMZN)
KUR_AMZN <- kurtosis(Merged_rtn_cc.xts$AMZN)
Qnorm_AMZN <- quantile(Merged_rtn_cc.xts$AMZN)

Mean_GOOG <- mean(Merged_rtn_cc.xts$GOOG)
Variance_GOOG <- var(Merged_rtn_cc.xts$GOOG)[1]
SD_GOOG <- sd(Merged_rtn_cc.xts$GOOG)
SK_GOOG <- skewness(Merged_rtn_cc.xts$GOOG)
KUR_GOOG <- kurtosis(Merged_rtn_cc.xts$GOOG)
Qnorm_GOOG <- quantile(Merged_rtn_cc.xts$GOOG)

#CALCOLO VARIANZA E COOVARIANZA

cov(Merged_rtn_cc.xts)
cor(Merged_rtn_cc.xts)

scatterPlot(Merged_rtn_cc.xts$AAPL, Merged_rtn_cc.xts$MSFT,Merged_rtn_cc.xts$AMZN,Merged_rtn_cc.xts$GOOG, c("AAPL","MSFT","AMZN","GOOG"))


#FORCASTING CON ARIMA
n <- 96
m <- 12
l <- 10
source("autoForcasting.R")
source("Forcasting.R")#Funziona ma ci mette molto tempo

#BETA COMPUTATION
source("Beta_comp.R")
beta_appl<-calcolo_beta(AAPL,SP500)
beta_msft<-calcolo_beta(MSFT,SP500)
beta_amzn<-calcolo_beta(AMZN,SP500)
beta_goog<-calcolo_beta(GOOG,SP500)
mostraGraficoBeta(AAPL_betas.xts, "Andamento Beta di Apple")
mostraGraficoBeta(MSFT_betas.xts, "Andamento Beta di Microsoft")
mostraGraficoBeta(AMZN_betas.xts, "Andamento Beta di Amazon")
mostraGraficoBeta(GOOG_betas.xts, "Andamento Beta di Google")

#calcolo il ritorno tramite beta
risk_free <- 0.01
ritornoB_AAPL <- calcolaRitornoBeta(Merged_betas$AAPL,risk_free,SP500)
ritornoB_MSFT <-calcolaRitornoBeta(Merged_betas$MSFT,risk_free,SP500)
ritornoB_AMZN <-calcolaRitornoBeta(Merged_betas$AMZN,risk_free,SP500)
ritornoB_GOOG <-calcolaRitornoBeta(Merged_betas$GOOG,risk_free,SP500)

#PORTFOLIO MANAGEMENT
budget_portafoglio <-50000

singola_transazione <- -0.01
peso_minimo <- 0.01
peso_massimo <- 1
source("Portfolio.R")
plot(j$EOP.Value, main="Andamento valore del mio Portafoglio")
#WEB APP

source("Server.R")
source("UI.R")
shinyApp(ui = ui, server = server)

