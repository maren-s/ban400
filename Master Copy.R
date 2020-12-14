
library(lubridate)
library(neuralnet)
library(BatchGetSymbols)
library(TTR)
library(ggplot2)
library(xts)
library(rugarch)
library(tidyverse)
library(aTSA)
library(sarima)
library(FinTS)

data_download <- function(ticker, days){
  #' Downloads data
  #' 
  #' Download stock price data given amount of days back in time
  #' 
  #' @param ticker the ticker to the stock one wishes to download data from
  #' @param days amount of days for the stock price data 
  to <- Sys.Date()
  from <- to - days
  data <- BatchGetSymbols(tickers = ticker, 
                          first.date = from,
                          last.date = to, 
                          freq.data = 'daily')
  return(data)
}


HAM <- function(ticker, days){
  #' Calculates volatility
  #' 
  #' Calculates volatility based on Historical Average Model
  #' 
  #' @param ticker the ticker to the stock one wishes to download data from
  #' @param days amount of days for the stock price data 
  data <- data_download(ticker, days)$df.tickers
  data$vol <- runSD(data$ret.closing.prices, n= 2)
  data$vol %>% na.omit() %>% mean() * sqrt(252)
}



## function for Neural Networks
NN <- function(ticker, days){
  #' Calculates volatility
  #' 
  #' Calculates volatility based on Neural Network
  #' 
  #' @param ticker the ticker to the stock one wishes to download data from
  #' @param days amount of days for the stock price data 
  if (days < 90){
    print("Choose more than 90 days for a more precise calculation")
  } else {
    data <- data_download(ticker, days)$df.tickers
    data$vol <- runSD(data$ret.closing.prices, n= 2) 
    data <- data[-2:-1,]
    data <- as.data.frame(data$vol)
    data[nrow(data) + 1, ] <- NA
    data$lag <- lag(data$`data$vol`)
    data <- data[-1,]
    trainset <- data[1:(nrow(data)-1), ]
    testset <- data[nrow(data), ]
    nn <- neuralnet(`data$vol` ~ lag, data=trainset, hidden=c(2,1), linear.output=FALSE, threshold=0.01)
    temp_test <- subset(testset, select = c("lag"))
    nn.results <- neuralnet::compute(nn, temp_test)
    results <- data.frame(actual = testset$`data$vol`, prediction = nn.results$net.result)
    print(results$prediction[nrow(results)] * sqrt(252)) 
  }
}

EWMA<- function(ticker, days) {
  #' Calculates volatility
  #' 
  #' Calculates volatility based on Exponentially Weighted Moving Average (EWMA)
  #' 
  #' @param ticker the ticker to the stock one wishes to download data from
  #' @param days amount of days for the stock price data 
  data <- data_download(ticker, days)$df.tickers
  logreturn <-  log(data$price.close/lag(data$price.close)) #take the log of returns
  weights <- (1-0.94)*0.94^(1:length(logreturn))  ##calculating the weights
  weights <- sort(weights,decreasing=FALSE) #arrange weights from least to greatest as return data is arranged from oldest to newest
  product <- weights * (logreturn^2)##multiply weights times squared log returns
  product %>% na.omit(.) %>% sum(.) %>% sqrt(.) * sqrt(252)
}


# GARCH MODEL

GARCH<- function(ticker, days){
  #' Calculates volatility
  #' 
  #' Calculates volatility based on Generalized Autoregressive Conditional Heteroskedasticity (GARCH)
  #' 
  #' @param ticker the ticker to the stock one wishes to download data from
  #' @param days amount of days for the stock price data 
  data <- data_download(ticker, days)$df.tickers
  rtn <- diff(log(data$price.close)) #take the log of returns
  rtn2 <- rtn^2 #Correlogram squared log-returns
  #ARMA-GARCH(1,1)
  spec <-  rugarch::ugarchspec(variance.model=list(model="sGARCH", garchOrder=c(1,1)),
                               mean.model=list(armaOrder=c(1,0), include.mean=TRUE))
  fit <- rugarch::ugarchfit(data = rtn , spec = spec) #Model Estimation
  forc <- rugarch::ugarchforecast(fit,n.ahead = 1) #Model Forecasting
  print(as.numeric(grep('[[:digit:]]\\.[[:digit:]]', forc@forecast$sigmaFor, value = TRUE)) * sqrt(252))
  if(ArchTest(rtn)$p.value < 0.05){
    print("Warning: No autoregressive conditional heteroscedasticity in the data")
  }
  if(Box.test(rtn2, type="Ljung-Box", lag = 12)$p.value > 0.05){
    print("Warning: No serial correlation in the data")
  }
}


RUN <- function(model, ticker, days){
  #'Run function
  #'
  #'Run function for the volatility models: GARCH, NN, EWMA and HAM
  #'
  #'@param ticker the ticker to the stock one wishes to download data from
  #' @param days amount of days for the stock price data 
  if(model == "GARCH"){
    GARCH(ticker, days)
  } else if (model == "HAM"){
    HAM(ticker, days)
  } else if (model == "NN"){
    NN(ticker, days)
  } else if (model == "EWMA"){
    EWMA(ticker, days)
  } else {
    print("Choose a model")
  }
}


