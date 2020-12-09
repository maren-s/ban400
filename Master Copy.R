
library(lubridate)
library(neuralnet)
library(BatchGetSymbols)
library(TTR)
library(ggplot2)
library(xts)
library(tidyverse)


data_download <- function(ticker, days){
  to <- Sys.Date()
  from <- to - days
  data <- BatchGetSymbols(tickers = ticker, 
                          first.date = from,
                          last.date = to, 
                          freq.data = 'daily')
  return(data)
}


## function for Neural Networks
NN <- function(ticker, days){
  if (days < 90){
    print("Choose more than 90 days for a more precise calculation")
  } else {
    data <- data_download(ticker, days)$df.tickers
    data$vol <- runSD(data$ret.closing.prices, n=20) * sqrt(252)
    data <- data[-30:-1,]
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
    print(results$prediction[nrow(results)])
  }
}

EWMA<- function(ticker, days) {
  data <- data_download(ticker, days)$df.tickers
  logreturn <-  log(data$price.close/lag(data$price.close)) #take the log of returns
  weights <- (1-0.94)*0.94^(1:length(logreturn))  ##calculating the weights
  weights <- sort(weights,decreasing=FALSE) #arrange weights from least to greatest as return data is arranged from oldest to newest
  product <- weights * (logreturn^2)##multiply weights times squared log returns
  product %>% na.omit(.) %>% sum(.) %>% sqrt(.) * sqrt(252) %>% print()
}



