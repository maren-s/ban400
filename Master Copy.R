
library(lubridate)
library(neuralnet)
library(BatchGetSymbols)
library(TTR)



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
    print("Choose more than 90 days for more precise calculations")
  } else {
    data <- data_download(ticker, days)$df.tickers
    data$vol <- runSD(data$ret.closing.prices, n=30) * sqrt(252)
    data <- data[-30:-1,]
    scaleddata <- as.data.frame(data$vol)
    scaleddata[nrow(scaleddata) + 1, ] <- NA
    scaleddata$lag <- lag(scaleddata$`data$vol`)
    scaleddata <- scaleddata[-1,]
    trainset <- scaleddata[1:(nrow(scaleddata)-1), ]
    testset <- scaleddata[nrow(scaleddata), ]
    nn <- neuralnet(`data$vol` ~ lag, data=trainset, hidden=c(2,1), linear.output=FALSE, threshold=0.01)
    temp_test <- subset(testset, select = c("lag"))
    nn.results <- neuralnet::compute(nn, temp_test)
    results <- data.frame(actual = testset$`data$vol`, prediction = nn.results$net.result)
    print(results$prediction[nrow(results)])
  }
}



