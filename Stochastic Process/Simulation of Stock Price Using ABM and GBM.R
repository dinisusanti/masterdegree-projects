#Importing the required library
library(quantmod)
library(timeSeries)
library(xts)
#Impoting monthly data from yahoo.finance
getSymbols("AAPL", return.class = 'xts', index.class = 'Date', from="2020-01-01", to="2021-12-31", periodicity = "monthly")
AAPLclose = AAPL[ , 4]
plot(AAPLclose, main = "Closing Price of AAPL", ylab = "Stock Price")

AAPL.ts = ts(AAPLclose) #changing into time series

#Creating Drift Function and calculating the drift of the time series
drift.f = function(S, lag=1){
  N = length(S)
  if (N < 1 + lag){
    stop("S must be greater than 2 + lag")
  }
  ct = S[(1+lag):N]
  pt = S[1:(N-lag)]
  t = 1
  dt = t/N
  stk.R =(ct-pt)/pt
  mu.hat = sum(stk.R)/(N*dt)
  mu.hat
}

drift.f(AAPL.ts)
drift = drift.f(AAPL.ts)

##Creating Diffusion Function and calculating the diffusion of the time series
vol.f = function (S, lag=1){
  N = length(S)
  if (N < 1 + lag){
    stop("S must be greater than 2 + lag")
  }
  ct = S[(1+lag):N]
  pt = S[1:(N-lag)]
  Diff = ct - pt
  tt = 1
  dt = tt/N
  stk.R = (ct-pt)/pt
  mu.hat = mean(stk.R)
  hat.sig2 = sum((stk.R-mu.hat)^2)/((N-1)*dt)
  hat.sig = sqrt(hat.sig2)
  hat.sig
}

vol.f(AAPL.ts)
diffusion = vol.f(AAPL.ts)

#Input and calculating Parameters
Nsim = 1000 #numbers of simulation
dT <- 1/Nsim #time horizon = 1
Time <- seq(from=0, to=1, by=dT) 
length(Time)
X0 <- AAPL.ts[1] #initial value for simulation 
z <- rnorm(Nsim, mean = 0, sd = 1) 
dw <- z*sqrt(dT)
w <- c(0, cumsum(dw))  

#ABM Simulation
X_ABM <- numeric(Nsim+1)
X_ABM[1] <- X0
for(i in 2:length(X_ABM)){
  X_ABM[i] <- X_ABM[i-1] + drift*dT + diffusion*dw[i-1]
}

plot(Time, X_ABM, type = "l", col = "darkgreen", main = "ABM Modeling of AAPL Closing Price", 
     xlab = expression("t"[i]), ylab = expression("W"[t[i]]))

#GBM Simulation
X_GBM <- numeric(Nsim+1)
X_GBM[1] <- X0
for(i in 2:length(X_GBM)){
  X_GBM[i] <- X_GBM[i-1] + drift*X_GBM[i-1]*dT + diffusion*X_GBM[i-1]*dw[i-1]
}

plot(Time, X_GBM, type = "l", col = "blue", main = "GBM Modeling of AAPL Closing Price", 
     xlab = expression("t"[i]), ylab = expression("W"[t[i]]))

