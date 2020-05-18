lb<-rep(peso_minimo, length(assets_name))
ub<-rep(peso_massimo, length(assets_name))
portfoliotop<-portfolio.optim(Merged_rtn_cc.xts, reslow = lb, reshigh = ub, rf=risk_free)
pesi<-portfoliotop$pw
names(pesi) <- assets_name
?barplot
barplot(pesi, main="Pesi degli Asset nel Portafoglio Ottimo")
p <- Return.portfolio(Merged_rtn_cc.xts, 
                      weights = pesi, 
                      verbose = TRUE, 
                      value = budget_portafoglio, 
                      rebalance_on = c("months"))

BW <- p$BOP.Weight
EW <- p$EOP.Weight
txns <- BW - lag(EW)
Monthly_To <- xts(rowSums(abs(txns[,1:4])), order.by = index(txns))
plot(Monthly_To, main="Monthly Trasaction Cost")

txn_costs <- Monthly_To * singola_transazione
rets_Txn_Cost <- p$returns + txn_costs
compare <- na.omit(cbind(p$returns,rets_Txn_Cost))
colnames(compare)<- c("No costi Transizione", "Con costi di Transizione")
print(table.AnnualizedReturns(compare))
j <-Return.portfolio(na.omit(rets_Txn_Cost), verbose = TRUE, value = budget_portafoglio,rebalance_on = c("months"))
plot(j$EOP.Value, main="Andamento valore del mio Portafoglio")
charts.PerformanceSummary(compare)

     