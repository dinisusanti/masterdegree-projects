#********************
#   ABM simulation   
#********************

#input parameter values
tau <- 1 #time horizon (year)
mu <- 0.05 #drift
sigma <- 0.15 #volatility
X0 <- 10 #initial value

#Simulate ABM Path with n = 50
N <- 50
dt <- tau/N
time <- seq(from=0, to=tau, by=dt) 
length(time)

Z <- rnorm(N, mean = 0, sd = 1) 
dW <- Z*sqrt(dt)
W <- c(0, cumsum(dW)) 

X_ABM <- numeric(N+1)
X_ABM[1] <- X0
for(i in 2:length(X_ABM)){
  X_ABM[i] <- X_ABM[i-1] + mu*dt + sigma*dW[i-1]
}

#Plot against time t
plot(time, X_ABM, type = "l", col = "blue", main = "ABM Path with n = 50", 
     xlab = expression("t"[i]), ylab = expression("W"[t[i]]))

#Simulate ABM Path with n = 2000
N1 <- 2000
dt1 <- tau/N1
time1 <- seq(from=0, to=tau, by=dt1) 
length(time1)

Z1 <- rnorm(N1, mean = 0, sd = 1) 
dW1 <- Z1*sqrt(dt1)
W1 <- c(0, cumsum(dW1)) 

X_ABM1 <- numeric(N1+1)
X_ABM1[1] <- X0
for(i in 2:length(X_ABM1)){
  X_ABM1[i] <- X_ABM1[i-1] + mu*dt1 + sigma*dW1[i-1]
}

#Plot against time t
plot(time1, X_ABM1, type = "l", col = "darkgreen", main = "ABM Path with n = 2000", 
     xlab = expression("t"[i]), ylab = expression("W"[t[i]]))

#********************
#   GBM simulation   
#********************

#Simulate GBM Path with n = 50
n <- 50
dT <- tau/n
Time <- seq(from=0, to=tau, by=dT) 
length(Time)

z <- rnorm(n, mean = 0, sd = 1) 
dw <- z*sqrt(dT)
w <- c(0, cumsum(dw))  

X_GBM <- numeric(n+1)
X_GBM[1] <- X0
for(i in 2:length(X_GBM)){
  X_GBM[i] <- X_GBM[i-1] + mu*X_GBM[i-1]*dT + sigma*X_GBM[i-1]*dw[i-1]
}

plot(Time, X_GBM, type = "l", col = "blue", main = "GBM path with n = 50", 
     xlab = expression("t"[i]), ylab = expression("W"[t[i]]))

#Simulate GBM Path with n = 2000
n1 <- 2000
dT1 <- tau/n1
Time1 <- seq(from=0, to=tau, by=dT1) 
length(Time1)

z1 <- rnorm(n1, mean = 0, sd = 1) 
dw1 <- z1*sqrt(dT1)
w1 <- c(0, cumsum(dw1))  

X_GBM1 <- numeric(n1+1)
X_GBM1[1] <- X0
for(i in 2:length(X_GBM1)){
  X_GBM1[i] <- X_GBM1[i-1] + mu*X_GBM1[i-1]*dT1 + sigma*X_GBM1[i-1]*dw1[i-1]
}

plot(Time1, X_GBM1, type = "l", col = "darkgreen", main = "GBM path with n = 2000", 
     xlab = expression("t"[i]), ylab = expression("W"[t[i]]))
