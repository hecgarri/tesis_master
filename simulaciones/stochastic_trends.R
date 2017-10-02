rm(list=ls())
if (!require(mvtnorm)) install.packages("mvtnorm"); require(mvtnorm)
if (!require(urca)) install.packages("urca"); require(urca)
if (!require(vars)) install.packages("vars"); require(vars)
if (!require(tsDyn)) install.packages("tsDyn"); require(tsDyn)

set.seed(1234)

n = 250

e = rmvnorm(n, mean=rep(0,3))

t = 1:n

u1.ar1 = 10+0.1*t+arima.sim(model = list(ar=0.75), n, innov = e[,1])

y2 = cumsum(e[,2]) # tendencia estocástica 

y1 = 0.8*y2+u1.ar1

ts.plot(y1,y2)

datos = cbind(y1,y2)

vecm = ca.jo(datos, ecdet = c("trend"))
summary(vecm)

vecm2 = VECM(datos, r=1,lag = 1, LRinclude = c("both"), estim = c("ML"))
summary(vecm2)

