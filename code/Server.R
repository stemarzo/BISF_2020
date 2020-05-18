server <- function(input, output) {

  output$plotAdjusted <- renderPlot({
    dati<-NULL
    nome<-NULL
    if (input$appl_c){
      nome<-cbind(nome,"AAPL")
      if(is.null(dati)){
        dati<-AAPL_adj_close.xts
      }else{
        dati<-merge(dati,APPL_adj_close.xts)
      }
    }
    if (input$msft_c){
      nome<-cbind(nome,"MSFT")
      if(is.null(dati)){
        dati<-MSFT_adj_close.xts
      }else{
        dati<-merge(dati,MSFT_adj_close.xts)
      }
    }
    if (input$amzn_c){
      nome<-cbind(nome,"AMZN")
      if(is.null(dati)){
        dati<-AMZN_adj_close.xts
      }else{
        dati<-merge(dati,AMZN_adj_close.xts)
      }
    }
    if (input$goog_c){
      nome<-cbind(nome,"GOOG")
      if(is.null(dati)){
        dati<-GOOG_adj_close.xts
      }else{
        dati<-merge(dati,GOOG_adj_close.xts)
      }
    }
    
    colnames(dati) <- nome
    mostraGrafico(dati,"Adjusted Close Price of Assets")
     
    
  })
  
  output$plotReturn <- renderPlot({
    dati<-NULL
    nome<-NULL
    if (input$appl_r){
      nome<-cbind(nome,"AAPL")
      if(is.null(dati)){
        dati<-AAPL
      }else{
        dati<-merge(dati,AAPL)
      }
    }
    if (input$msft_r){
      nome<-cbind(nome,"MSFT")
      if(is.null(dati)){
        dati<-MSFT
      }else{
        dati<-merge(dati,MSFT)
      }
    }
    if (input$amzn_r){
      nome<-cbind(nome,"AMZN")
      if(is.null(dati)){
        dati<-AMZN
      }else{
        dati<-merge(dati,AMZN)
      }
    }
    if (input$goog_r){
      nome<-cbind(nome,"GOOG")
      if(is.null(dati)){
        dati<-GOOG
      }else{
        dati<-merge(dati,GOOG)
      }
    }
    
    colnames(dati) <- nome
    mostraGrafico(dati,"CC Return of the Assets")
    
    
  })
  
  
  output$plotHisto <- renderPlot({
    if(input$ass_l=="AAPL"){
      mostraIstogramma("AAPL", "Distribuzione di CC Return abount AAPL", Merged_rtn_cc.xts$AAPL,"#e22f7b", "white", c(0,6), c(-0.2,0.2))
      if(input$densita){
        points(density(Merged_rtn_cc.xts$AAPL), type="l", col="black")
      }
    }
    if(input$ass_l=="MSFT"){
      mostraIstogramma("MSFT", "Distribuzione di CC Return abount MSFT", Merged_rtn_cc.xts$MSFT,"#2fbe81", "white", c(0,9), c(-0.2,0.2))
      if(input$densita){
        points(density(Merged_rtn_cc.xts$MSFT), type="l", col="black")
      }
    }
    if(input$ass_l=="AMZN"){
      mostraIstogramma("AMZN", "Distribuzione di CC Return abount AMZN", Merged_rtn_cc.xts$AMZN,"#12b1ff", "white", c(0,8), c(-0.15,0.2))
      if(input$densita){
        points(density(Merged_rtn_cc.xts$AMZN), type="l", col="black")
      }
    }
    if(input$ass_l=="GOOG"){
      mostraIstogramma("GOOG", "Distribuzione di CC Return abount GOOG", Merged_rtn_cc.xts$GOOG,"#ffcd12", "white", c(0,7), c(-0.25,0.25))
      if(input$densita){
        points(density(Merged_rtn_cc.xts$GOOG), type="l", col="black")
      }
    }
    
  })
  
  output$plotBox <- renderPlot({
    if(input$ass_bp=="AAPL"){
      boxplot(coredata(AAPL), 
              main= paste("Boxplot of mothly cc returns APPL"),
              ylab="monthly cc return", 
              col="#e22f7b")
    }
    if(input$ass_bp=="MSFT"){
      boxplot(coredata(MSFT), 
              main= paste("Boxplot of mothly cc returns MSFT"),
              ylab="monthly cc return", 
              col="#2fbe81")
    }
    if(input$ass_bp=="AMZN"){
      boxplot(coredata(AMZN), 
              main= paste("Boxplot of mothly cc returns AMZN"),
              ylab="monthly cc return", 
              col="#12b1ff")
    }
    if(input$ass_bp=="GOOG"){
      boxplot(coredata(GOOG), 
              main= paste("Boxplot of mothly cc returns GOOG"),
              ylab="monthly cc return", 
              col="#ffcd12")
    }
    
  })
  
  output$plotQQ <- renderPlot({
    if(input$ass_qq=="AAPL"){
      qqnorm(AAPL, main="AAPL", col="#e22f7b")
      if(input$qqline){
        qqline(AAPL)
      }
    }
    if(input$ass_qq=="MSFT"){
      qqnorm(AAPL, main="MSFT", col="#2fbe81")
      if(input$qqline){
        qqline(MSFT)
      }
    }
    if(input$ass_qq=="AMZN"){
      qqnorm(AAPL, main="AMZN", col="#12b1ff")
      if(input$qqline){
        qqline(AMZN)
      }
    }
    if(input$ass_qq=="GOOG"){
      qqnorm(AAPL, main="GOOG", col="#ffcd12")
      if(input$qqline){
        qqline(GOOG)
      }
    }
  })
  
  output$plotDiagn <- renderPlot({
    aapl<-NULL
    msft<-NULL
    amzn<-NULL
    goog<-NULL
    nome<-NULL
    if (input$appl_d){
      nome<-cbind(nome,"AAPL")
      aapl<-AAPL
    }
    if (input$msft_d){
      nome<-cbind(nome,"MSFT")
      msft<-MSFT
    }
    if (input$amzn_d){
      nome<-cbind(nome,"AMZN")
      amzn<-AMZN
    }
    if (input$goog_d){
      nome<-cbind(nome,"GOOG")
      goog<-GOOG
    }
    
    scatterPlot(aapl,msft,amzn,goog,nome)
    
    
  })
  
}


