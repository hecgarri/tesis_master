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
for (j in 1:k)  z2 <- cbind(z2,x[(2+k-j):(n-j),]-x[(1+k-j):(n-1-j),])



crossprod(z2,z2)%*%crossprod(z2,z0)