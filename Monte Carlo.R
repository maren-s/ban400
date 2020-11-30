library(readxl)
library(ggplot2)
library(xts)

GSPC <- read_excel("GSPC.xlsx", col_types = c("date","numeric", "numeric", "numeric", "numeric", "numeric", "numeric"))


#### Kaggle https://www.kaggle.com/tandonarpit6/predicting-nifty-using-monte-carlo-fbprophet
GSPC$returns=(GSPC$`Adj Close`/ GSPC$Open)-1

volatility= sd(GSPC$returns)
trading_days=nrow.as.numeric(GSPC)
as.numeric(trading_days) 

###mean=(nifty.loc[trading_days-1,'Close']/nifty.loc[0,'Open'])-1
mean=(mean(GSPC$`Adj Close`/GSPC$Open)-1)

daily_returns=dnorm(trading_days, mean/trading_days, volatility)
##daily_returns=np.random.normal(mean/trading_days,volatility,trading_days)+1

index_returns=(10980)

for (x in daily_returns) {
  append(index_returns,index_returns[-1]*x)
}

##Let's run the random distributions for 1000 times.
for (i in range(1000)) {
    daily_returns=dnorm(trading_days, mean/trading_days, volatility)+1

    index_returns=(10980) 
    
    for (x in daily_returns){
      append(index_returns,index_returns[-1]*x)
    }
        
    plt.plot(index_returns)
}

plt.show()
