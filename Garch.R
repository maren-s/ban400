# GARCH MODEL
library(rugarch)

GARCH<- function(){
  GSPC.rtn <- diff(log(GSPC$GSPC.Adjusted)) #take the log of returns
  GSPC.rtn <- GSPC.rtn[-1,]
  GSPC.rtn2 <- GSPC.rtn^2 #Correlogram squared log-returns
  
  #ARMA-GARCH(1,1)
  spec <-  rugarch::ugarchspec(variance.model=list(model="sGARCH", garchOrder=c(1,1)),
                               mean.model=list(armaOrder=c(1,0), include.mean=TRUE))
  fit <- rugarch::ugarchfit(data = 100*GSPC.rtn , spec = spec) #Model Estimation
  forc <- rugarch::ugarchforecast(fit,n.ahead = 10) #Model Forecasting
  print(forc)
}