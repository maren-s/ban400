library(readxl)
library(ggplot2)
library(xts)

GSPC <- read_excel("GSPC.xlsx", col_types = c("date","numeric", "numeric", "numeric", "numeric", "numeric", "numeric"))

###EMWA method

#volatiliy² = lambda+(1-lambda)u²_n-1
#first step: take log
#second: square the returns: returns_sq/number of days
#third: assign weights: lambda is smoothing constant, e.g. 0.94
#fourth: multiply returns-squared with weights
#fifth: take summation of Return²*weights
EWMA<- function(x,lambda) 
{
logreturn = log(GSPC$returns) #take the log of returns

#weights<-(1-lambda)*lambda^z ##calculating the weights

weights<-(1-0.94)*0.94^z ##calculating the weights
z<-as.matrix(trading_days) ##convert from numeric to matrix

weights<-sort(weights,decreasing=FALSE) #arrange weights from least to greatest as return data is arranged from oldest to newest
product<-weights*logreturn*logreturn ##multiply weights times squared log returns

product<-as.matrix(product) ##convert to matrix

product<-na.omit(product) ##remove all Nas in data

Variance<-colSums(product) ##sum the product 

Volatility<-sqrt(Variance)  

final<-cbind(Variance,Volatility) ##combine columns of Variance and Volatility
}

predicted<-EWMA(x,.94) #test the function

predicted ##it works!!

### actual volatility 
GSPC$returns=(GSPC$`Adj Close`/ GSPC$Open)-1

volatility= sd(GSPC$returns)
trading_days=nrow(GSPC)
as.numeric(trading_days) 

actual=GSPC$returns-mean(GSPC$returns)


### test prediction with root mean squared error
sqrt(mean((predicted-actual)^2))
