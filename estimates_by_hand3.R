rm(list=ls())
setwd("/home/hector/GoogleDrivePersonal/Master/Tesis/GitHub/tesis_master")
source("weekly_palta.R")

if(!require(forecast)) install.packages("forecast"); require(forecast)
y1 = precio_mayorista 
y2 = precio_supermercado


fit1 = auto.arima(precio_mayorista, seasonal=FALSE)
# Filtro de Kalman 
kr1 = KalmanRun(precio_mayorista, fit1$model)
id.na1 = which(is.na(precio_mayorista))
y1 = precio_mayorista
for (i in id.na1){
  y1[i] = fit1$model$Z %*% kr1$states[i,]
}
y1[id.na1]  

fit2 = auto.arima(precio_supermercado, seasonal=FALSE)
# Filtro de Kalman 
kr2 = KalmanRun(precio_supermercado, fit2$model)
id.na2 = which(is.na(precio_supermercado))
y2 = precio_supermercado
for (i in id.na2){
  y2[i] = fit2$model$Z %*% kr2$states[i,]
}
y2[id.na2]  

###########################################################################
##
##  TAR_CI.R
##  To estimate and test a threshold bi-variate VECM
## 
##  written by:
##  
##  Bruce E. Hansen
##  Department of Economics
##  Social Science Building
##  University of Wisconsin
##  Madison, WI 53706-1393
##  behansen@wisc.edu
##  http://www.ssc.wisc.edu/~bhansen/
##  
##  and 
##  
##  Byeongseon Seo
##  Department of Economics
##  Soongsil University
##  Seoul, 156-743
##  Korea
##  seo@saint.soongsil.ac.kr
##  
##  
##  This R program estimates a bi-variate VECM, a threshold bi-variate VECM, and
##  tests for the presence of a threshold.  The methods are those described in
##  "Testing for Threshold Cointegration" by Bruce E. Hansen and Byeongseon Seo.
##  
##  The program is set up to replicate the empirical application to the 3-month 
##  and 6-month interest rates series.  For your own application, load your data 
##  into the matrix "dat", and change the controls listed below
##  
###########################################################################

datos = cbind(y2,y1) %>% as.matrix
colnames(datos) = c("Supermercados", "Mayoristas")

# En este apartado utilizo la otación sugerida por Juselius (2006)
x = datos 
k = 7
n = nrow(datos)
z0 = as.matrix(x[(2+k):n,]-x[(1+k):(n-1),])
z1 = x[(1+k):(n-1),]
z2 = NULL
t = nrow(z0)
z2 = 1
for (j in 1:k)  z2 <- cbind(x[(2+k-j):(n-j),]-x[(1+k-j):(n-1-j),],z2)

r0 = resid(lm(z0~z2))
r1 = resid(lm(z1~z2))

s00 = crossprod(r0,r0)/t
s01 = crossprod(r0,r1)/t
s10 = crossprod(r1,r0)/t
s11 = crossprod(r1,r1)/t

ev = eigen(solve(t(r1)%*%r1)%*%(t(r1)%*%r0)%*%solve(t(r0)%*%r0)%*%(t(r0)%*%r1))
va = ev$values
ve = ev$vectors
