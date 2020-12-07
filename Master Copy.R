
library(lubridate)
library(neuralnet)
library(BatchGetSymbols)



data_download <- function(ticker, days){
  to <- Sys.Date()
  from <- to - days
  data <- BatchGetSymbols(tickers = ticker, 
                  first.date = from,
                  last.date = to, 
                  freq.data = 'daily')
  return(data)
}


#^GSPC for SP500
data <- data_download("^GSPC", 395)$df.tickers #use 395 - one year + one month for volatility calculation

data$vol <- runSD(data$ret.closing.prices, n=30) * sqrt(252)
data <- data[-30:-1,]


#Neural Network for volatility forecasting

scaleddata <- as.data.frame(data$vol)
scaleddata[nrow(scaleddata) + 1, ] <- NA
scaleddata$lag <- lag(scaleddata$`data$vol`)
scaleddata <- scaleddata[-1,]

trainset <- scaleddata[1:(nrow(scaleddata)*0.8), ]
testset <- scaleddata[(nrow(scaleddata)*0.8+1):(nrow(scaleddata)+1), ]

nn <- neuralnet(`data$vol` ~ lag, data=trainset, hidden=c(2,1), linear.output=FALSE, threshold=0.01)


temp_test <- subset(testset, select = c("lag"))

head(temp_test)


nn.results <- compute(nn, temp_test)

results <- data.frame(actual = testset$`data$vol`, prediction = nn.results$net.result)


#print predicted volatility
print(results$prediction[nrow(results)])


## function
NN <- function(){
  data$vol <- runSD(data$ret.closing.prices, n=30) * sqrt(252)
  data <- data[-30:-1,]
  scaleddata <- as.data.frame(data$vol)
  scaleddata[nrow(scaleddata) + 1, ] <- NA
  scaleddata$lag <- lag(scaleddata$`data$vol`)
  scaleddata <- scaleddata[-1,]
  trainset <- scaleddata[1:(nrow(scaleddata)*0.8), ]
  testset <- scaleddata[(nrow(scaleddata)*0.8+1):(nrow(scaleddata)+1), ]
  nn <- neuralnet(`data$vol` ~ lag, data=trainset, hidden=c(2,1), linear.output=FALSE, threshold=0.01)
  temp_test <- subset(testset, select = c("lag"))
  nn.results <- compute(nn, temp_test)
  results <- data.frame(actual = testset$`data$vol`, prediction = nn.results$net.result)
  print(results$prediction[nrow(results)])
}






