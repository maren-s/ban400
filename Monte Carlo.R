install.packages("fGarch")
install.packages("xts")
install.packages("xts", repos="http://cloud.r-project.org")

library(readxl)
library(ggplot2)
library(xts)

GSPC <- read_excel("GSPC.xlsx", col_types = c("date","numeric", "numeric", "numeric", "numeric", "numeric", "numeric"))

enddate = as.Date("2020-11-25") 

spec     = ugarchspec(variance.model=list(model="sGARCH",garchOrder=c(1,1)),
                     mean.model=list(armaOrder=c(0,0), include.mean=FALSE))
fitgarch = ugarchfit(spec = spec, data = window(GSPC$`Adj Close`)) 

###Kaggle
# Define variables
close <- GSPC$`Adj Close`
sigma <- 280.02
n <- 10000

# Monte Carlo method 
B <- 1000 # Number of iterations 
results <- replicate(B,{ 
  temp <-sample(n,sigma, replace = TRUE)
 sum(temp)})

#Input
S0 <- 3303
T <- 3/12
b <- K <- 3220
rf <- 0.002
f <- 0.01
n <- 5000
dt <- 1/250 * 0.5

simulate.path <- function(close, sigma, dt, T){
  t <- seq(dt, T, by = dt) #divides the time of today until T, in chops of time
  for(i in 1:length(t)){ # length of t is every time point we have
    St <- S[length(S)]*exp((rf-0.5*sigma^2)*dt + sigma*sqrt(t)*rnorm(1))
    St <- S[length(S)]*exp((rf-0.5*sigma^2)*dt + sigma*sqrt(dt)*rnorm(1))
    S <- c(S, St)
  }
  return(S)
}


# Comparison between calculated result and Monte Carlo result
print("Calculated result: ") 
print(n_loans * default_rate * default_loss)
print("Monte Carlo result: ")
print(mean(results))


####Marens notes

#Simulate 1 path of the index value
simulate.path <- function(S0, rf, sigma, dt, T){
  S <- S0
  t <- seq(dt, T, by = dt) #divides the time of today until T, in chops of time
  for(i in 1:length(t)){ # length of t is every time point we have
    St <- S[length(S)]*exp((rf-0.5*sigma^2)*dt + sigma*sqrt(t)*rnorm(1))
    St <- S[length(S)]*exp((rf-0.5*sigma^2)*dt + sigma*sqrt(dt)*rnorm(1))
    S <- c(S, St)
  }
  return(S)
}

sigma <- 0.16
S <- simulate.path(S0, rf, sigma, dt, T)
plot(S, type = "l")
abline(h=b)

### r pubs by r studio
set.seed(6360)
# S <- 1065
r <- 0.0125
vol <- 0.2
a <- 0.95
 c <- 0.15
 p <- -0.3
K <- seq(100,2000,100)
TTM <- 1
t <- 1/250
shock_1 <- matrix(0, ncol = 1000, nrow = 250)
for (i in 1:ncol(shock_1)) {
  shock_1[,i] <- rnorm(250, mean = 0, sd = 1)
 } 
 shock_2 <- matrix(0, ncol = 1000, nrow = 250)
 for (i in 1:ncol(shock_2)) {
  shock_2[,i] <- rnorm(250, mean = 0, sd = 1)
 }

### MC Simulation of Underlying

## Model Stochastic Volatility Process

vol_matrix <- matrix(0, ncol = 1000, nrow = 250)
colnames(vol_matrix) <- paste("Sim", 1:ncol(vol_matrix))
rownames(vol_matrix) <- paste("Day", 1:250)
vol_null <- vol^2
for (j in 1:ncol(vol_matrix)) {
  
  vol_matrix[1,j] <- vol_null+c*sqrt(t*abs(vol_null))*(p*shock_1[1,j]+sqrt(1-p^2)*shock_2[1,j])
  
  for (i in 2:nrow(vol_matrix)) {
    vol_matrix[i,j] <- vol_matrix[i-1,j] + a*(vol_null-vol_matrix[i-1,j])*t+c*
      sqrt(t*abs(vol_matrix[i-1,j]))*(p*shock_1[i,j] + sqrt(1-p^2)*shock_2[i,j])
  }
}

#Additional assumptions in this space include:
#Annualized long term volatility for the index is 20% per annum.
#The 1-year LIBOR rate is 1.25%
#For the stochastic volatility process, assume the initial drift coefficients, 
#a and c, are 0.95 and 0.85, respectively.

######## own try ##############
### r pubs by r studio
set.seed(6360)
S <- 1065
r <- 0.0125 # risk-free rate
vol <- 0.2 # annualized long term volatility
a <- 0.95 # drift coefficient
c <- 0.15 # (1- initial drift coefficient)
p <- -0.3 # correlation coefficient
K <- seq(100,2000,100)
TTM <- 1
t <- 1/250
shock_1 <- matrix(0, ncol = 1000, nrow = 250)
for (i in 1:ncol(shock_1)) {
  shock_1[,i] <- rnorm(250, mean = 0, sd = 1)
} 
shock_2 <- matrix(0, ncol = 1000, nrow = 250)
for (i in 1:ncol(shock_2)) {
  shock_2[,i] <- rnorm(250, mean = 0, sd = 1)
}

### MC Simulation of Underlying
## Model Stochastic Volatility Process
vol_matrix <- matrix(0, ncol = 1000, nrow = 250)
colnames(vol_matrix) <- paste("Sim", 1:ncol(vol_matrix))
rownames(vol_matrix) <- paste("Day", 1:250)
vol_null <- vol^2
for (j in 1:ncol(vol_matrix)) {
  
  vol_matrix[1,j] <- vol_null+c*sqrt(t*abs(vol_null))*(p*shock_1[1,j]+sqrt(1-p^2)*shock_2[1,j])
  
  for (i in 2:nrow(vol_matrix)) {
    vol_matrix[i,j] <- vol_matrix[i-1,j] + a*(vol_null-vol_matrix[i-1,j])*t+c*
      sqrt(t*abs(vol_matrix[i-1,j]))*(p*shock_1[i,j] + sqrt(1-p^2)*shock_2[i,j])
  }
}

#### Kaggle https://www.kaggle.com/tandonarpit6/predicting-nifty-using-monte-carlo-fbprophet
returns=(GSPC$`Adj Close`/ GSPC$Open)-1

volatility= sd(returns)
trading_days=length(returns)

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
