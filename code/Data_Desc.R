
scarica_dati <-function(nome_assets, start_stream, end_stream){
  Assets.xts <- getSymbols(nome_assets, from=start_stream, to=end_stream, src='yahoo', auto.assign = FALSE)
  Assets <- to.monthly(Assets.xts)
  Assets_adj_close <- Assets$Assets.xts.Adjusted
}

mostraGrafico <- function(dati, titolo){
  data_frame<-data.frame(date = index(dati), coredata(dati))
  melt_data <- melt(data_frame, id = "date")
  ggplot(data = melt_data, aes(x = as.Date(date), y = value, colour = variable)) +
    geom_line(size = 1) +
    scale_x_date() +
    scale_y_continuous() +
    labs(y = "Value", x = "Data", colour = "", title = titolo)+
    theme_minimal()
}

mostraIstogramma <- function(nome, titolo, dati, colore, colore_bordi, limiti_y, limiti_x){
  hist(dati,
       freq = FALSE,
       main = titolo, 
       col = colore,
       border = colore_bordi,
       xlab = nome,
       ylim = limiti_y,
       xlim = limiti_x)
}

multiplePlot <- function(nome, titolo, dati, colore, colore_bordi, limiti_y, limiti_x){
  par(mfrow=c(2,2)) 
  mostraIstogramma(nome, titolo, dati, colore, colore_bordi, limiti_y, limiti_x)
  plot(density(dati) , type="l", col=colore, border=colore_bordi, main=titolo,xlab = nome,
       ylim = limiti_y, xlim = limiti_x)
  boxplot(coredata(dati), 
          main= paste("Boxplot of mothly cc returns",nome),
          ylab="monthly cc return", 
          col=colore)
  qqnorm(dati, main="AAPL", col=colore)
  qqline(dati)
}


scatterPlot <- function(primo, secondo, terzo, quarto, nomi){
  primo.mat <- as.numeric(primo)
  secondo.mat <- as.numeric(secondo)
  terzo.mat <- as.numeric(terzo)
  quarto.mat <- as.numeric(quarto)
  par(mfrow=c(1,1))
  valori<-cbind(primo.mat, secondo.mat, terzo.mat, quarto.mat)
  colnames(valori)<-nomi
  pairs(valori, pch=18, 
        col="blue", main="Asset Correlation")
  
}

