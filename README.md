# ban400
**Project Description**

We want to create an app that allows the user see the forecast of the volatility of stocks alone and of the whole stock index using different methods.  The user should be able to easily change between stocks and the different methods. The user can also choose between different amount of days the forecasted volatility is calculated on. The data can be updated so that always the latest available figures are used for the calculation and the user will be notified if there are any problems with the data.  

The code gives four options to calculate it: 

*  Historical Average Model (HAM)

* Neural Networks (NN)

* Exponentially Weighted Moving Average Volatility (EMWA) 

* General autoregressive conditional heteroskedasticity models (GARCH) 

 We will use data from Yahoo Finance for the S&P 500
 
 ## Libraries
 
 we loaded the following librarires
 
 ```
 install.packages(c(lubridate, neuralnet, BatchGetSymbols, TTR, ggplot2, xts, rugarch, tidyverse, aTSA, sarima, FinTS))
 ```
 
First, we create a function that downloads the data and the user can choose between different amount of days the forecasted volatility is calculated 
 
 ```
data_download <- function(ticker, days) {
  #' Downloads data
  #'
  #' Download stock price data given amount of days back in time
  #'
  #' @param ticker the ticker to the stock one wishes to download data from
  #' @param days amount of days for the stock price data
  to <- Sys.Date()
  from <- to - days
  data <- BatchGetSymbols(
    tickers = ticker,
    first.date = from,
    last.date = to,
    freq.data = 'daily'
  )
  return(data)
}
 ```

 We created functions for each model 
 
 ##  Historical Average Model (HAM)
 
 ```
 HAM <- function(ticker, days) {
  #' Calculates volatility
  #'
  #' Calculates volatility based on Historical Average Model
  #'
  #' @param ticker the ticker to the stock one wishes to download data from
  #' @param days amount of days for the stock price data
  data <- data_download(ticker, days)$df.tickers
  data$vol <- runSD(data$ret.closing.prices, n = 2)
  data$vol %>% na.omit() %>% mean() * sqrt(252)
}
 ```
 
 ## Neural Networks (NN)
 
 ```
 NN <- function(ticker, days) {
  #' Calculates volatility
  #'
  #' Calculates volatility based on Neural Network
  #'
  #' @param ticker the ticker to the stock one wishes to download data from
  #' @param days amount of days for the stock price data
  if (days < 90) {
    print("Choose more than 90 days for a more precise calculation")
  } else {
    data <- data_download(ticker, days)$df.tickers
    data$vol <- runSD(data$ret.closing.prices, n = 2)
    data <- data[-2:-1, ]
    data <- as.data.frame(data$vol)
    data[nrow(data) + 1,] <- NA
    data$lag <- lag(data$`data$vol`)
    data <- data[-1, ]
    trainset <- data[1:(nrow(data) - 1),]
    testset <- data[nrow(data),]
    nn <-
      neuralnet(
        `data$vol` ~ lag,
        data = trainset,
        hidden = c(2, 1),
        linear.output = FALSE,
        threshold = 0.01
      )
    temp_test <- subset(testset, select = c("lag"))
    nn.results <- neuralnet::compute(nn, temp_test)
    results <-
      data.frame(actual = testset$`data$vol`,
                 prediction = nn.results$net.result)
    print(results$prediction[nrow(results)] * sqrt(252))
  }
}
 ```

## Exponentially Weighted Moving Average Volatility (EMWA) 

```
EWMA <- function(ticker, days) {
  #' Calculates volatility
  #'
  #' Calculates volatility based on Exponentially Weighted Moving Average (EWMA)
  #'
  #' @param ticker the ticker to the stock one wishes to download data from
  #' @param days amount of days for the stock price data
  data <- data_download(ticker, days)$df.tickers
  logreturn <-
    log(data$price.close / lag(data$price.close)) #take the log of returns
  weights <-
    (1 - 0.94) * 0.94 ^ (1:length(logreturn))  ##calculating the weights
  weights <-
    sort(weights, decreasing = FALSE) #arrange weights from least to greatest as return data is arranged from oldest to newest
  product <-
    weights * (logreturn ^ 2)##multiply weights times squared log returns
  product %>% na.omit(.) %>% sum(.) %>% sqrt(.) * sqrt(252)
}
```

## General autoregressive conditional heteroskedasticity models (GARCH)

```
GARCH <- function(ticker, days) {
  #' Calculates volatility
  #'
  #' Calculates volatility based on Generalized Autoregressive Conditional Heteroskedasticity (GARCH)
  #'
  #' @param ticker the ticker to the stock one wishes to download data from
  #' @param days amount of days for the stock price data
  data <- data_download(ticker, days)$df.tickers
  rtn <- diff(log(data$price.close)) #take the log of returns
  rtn2 <- rtn ^ 2 #Correlogram squared log-returns
  #ARMA-GARCH(1,1)
  spec <-
    rugarch::ugarchspec(
      variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
      mean.model = list(armaOrder = c(1, 0), include.mean =
                          TRUE)
    )
  fit <-
    rugarch::ugarchfit(data = rtn , spec = spec) #Model Estimation
  forc <-
    rugarch::ugarchforecast(fit, n.ahead = 1) #Model Forecasting
  print(as.numeric(
    grep(
      '[[:digit:]]\\.[[:digit:]]',
      forc@forecast$sigmaFor,
      value = TRUE
    )
  ) * sqrt(252))
  if (ArchTest(rtn)$p.value < 0.05) {
    print("Warning: No autoregressive conditional heteroscedasticity in the data")
  }
  if (Box.test(rtn2, type = "Ljung-Box", lag = 12)$p.value > 0.05) {
    print("Warning: No serial correlation in the data")
  }
}
```

Finally, we created a function that rus this codes and allows the user choose preferred method
```
RUN <- function(model, ticker, days) {
  #'Run function
  #'
  #'Run function for the volatility models: GARCH, NN, EWMA and HAM
  #'
  #'@param ticker the ticker to the stock one wishes to download data from
  #' @param days amount of days for the stock price data
  if (model == "GARCH") {
    GARCH(ticker, days)
  } else if (model == "HAM") {
    HAM(ticker, days)
  } else if (model == "NN") {
    NN(ticker, days)
  } else if (model == "EWMA") {
    EWMA(ticker, days)
  } else {
    print("Choose a model")
  }
}
```
