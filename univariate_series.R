##  univariate_series.R
##  
##  Héctor Garrido Henríquez
##  Analista Cuantitativo. Observatorio Laboral Ñuble
##  Docente. Facultad de Ciencias Empresariales
##  Universidad del Bío-Bío
##  Avenida Andrés Bello 720, Casilla 447, Chillán
##  Teléfono: +56-942353973
##  http://www.observatoriolaboralnuble.cl
##
##  Este programa se utiliza para el capítulo análisis de las series 
##  univariadas. 

setwd("/home/hector/GoogleDrivePersonal/Master/Tesis/GitHub/tesis_master")
source("weekly_palta.R")

if (!require(vars)) install.packages("vars"); require(vars)
if (!require(tsDyn)) install.packages("tsDyn"); require(vars)
if (!require(ggplot2)) installed.packages("ggplot2"); require(ggplot2)
if (!require(tidyverse)) install.packages("tidyverse"); require(tidyverse)
if (!require(forecast)) install.packages("forecast"); require(forecast)

y1 = precio_mayorista
y2 = precio_supermercado


par(family="serif", bty="l", bg="white", cex.lab=1) # opciones gráficas
ts.plot(precio_mayorista, precio_supermercado, lty=1:2,
        lwd=1, ylab="$/kilo", xlab="Tiempo")
legend("topleft", legend = c("Mayorista", "Supermercado"),lty=1:2, lwd=2, cex=0.8)


## ----results='hide', echo=FALSE, message=FALSE---------------------------


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


## ----fig-3,echo=F,fig.cap='Imputación de valores perdidos a través del filtro de Kalman', out.width='6.5in', out.height='4.5in', fig.align='center', fig.pos='H'----

ts.plot(precio_mayorista,precio_supermercado, lty=1:2 ,ylab="log(Precio ($/kilo))", 
        xlab="Tiempo")
lines(y1, lwd=1, lty=1)
points(time(y1)[id.na1], y1[id.na1], col = "red", pch = 18, cex=1)
points(time(y2)[id.na2], y2[id.na2], col = "blue", pch = 18, cex=1)
legend("topleft", legend = c("Valores imputados supermercados",
                             "Valores imputados mayoristas"), 
       col = c("red","blue"), pch = c(18), cex=0.8)


## ----fig-2.1,echo=F,fig.cap='Evolución del logaritmo del precio mayorista de la palta, 2008-2016\\label{fig5.1}', out.width='4.5in', out.height='3.5in', fig.align='center', fig.pos='!htpb'----
par(family="serif", bty="l")
layout(matrix(c(1,1,1,1,1,
                2,2,0,3,3),ncol = 5, byrow=TRUE))
plot(y1, main="a) Evolución log(precios) mayoristas, 2008-2016", 
     ylab="$/kilo")
Acf(y1, main="b) Función de autocorrelación", lag.max = 52)
Pacf(y1, main="c) Función de autocorrelación parcial", lag.max = 52)

## ----fig-2.2,echo=F,fig.cap='Evolución del logaritmo del precio en supermercado de la palta, 2008-2016\\label{fig5.2}', out.width='4.5in', out.height='3.5in', fig.align='center', fig.pos='!htpb'----
par(family="serif", bty="l")
layout(matrix(c(1,1,1,1,1,
                2,2,0,3,3),ncol =5, byrow=TRUE))
plot(y2, main="a) Evolución log(precios) supermercado, 2008-2016", 
     ylab="$/kilo")
Acf(y2, main="b) Función de autocorrelación", lag.max = 52)
Pacf(y2, main="c) Función de autocorrelación parcial", lag.max = 52)

## ----fig-2.3,echo=F,fig.cap='Evolución del logaritmo del precio mayorista de la palta, 2008-2016\\label{fig5.3}', out.width='4.5in', out.height='3.5in', fig.align='center', fig.pos='!htpb'----
par(family="serif", bty="l")
layout(matrix(c(1,1,1,1,1,
                2,2,0,3,3),ncol = 5, byrow=TRUE))
plot(diff(y1), main="a) Evolución log(precios) mayoristas, 2008-2016", 
     ylab="$/kilo")
Acf(diff(y1), main="b) Función de autocorrelación", lag.max = 52)
Pacf(diff(y1), main="c) Función de autocorrelación parcial", lag.max = 52)

## ----fig-2.4,echo=F,fig.cap='Evolución del logaritmo del precio en supermercado de la palta, 2008-2016\\label{fig5.4}', out.width='4.5in', out.height='3.5in', fig.align='center', fig.pos='!htpb'----
par(family="serif", bty="l")
layout(matrix(c(1,1,1,1,1,
                2,2,0,3,3),ncol =5, byrow=TRUE))
plot(diff(y2), main="a) Evolución log(precios) supermercado, 2008-2016", 
     ylab="$/kilo")
Acf(diff(y2), main="b) Función de autocorrelación", lag.max = 52)
Pacf(diff(y2), main="c) Función de autocorrelación parcial", lag.max = 52)

## ----echo=FALSE, include=FALSE-------------------------------------------
if (!require(urca)) install.packages("urca"); require(urca)

dickey11 = ur.df(y1, type="drift", selectlags = c("BIC"))
dickey21 = ur.df(y2, type="drift", selectlags = c("BIC"))
summary(dickey11)
summary(dickey21)


## ----echo=FALSE, include=FALSE-------------------------------------------
dickey1 = ur.df(y1, type="trend", selectlags = c("BIC"))
dickey2 = ur.df(y2, type="trend", selectlags = c("BIC"))
summary(dickey1)
summary(dickey2)


## ----include=FALSE-------------------------------------------------------
pperron1 =ur.pp(y1, type=c("Z-alpha"), model = c("trend"), lags = c("short")) 

pperron2 =ur.pp(y2, type=c("Z-alpha"), model = c("trend"), lags = c("short")) 


## ----include=FALSE-------------------------------------------------------
pperron11 =ur.pp(y1, type=c("Z-alpha"), model = c("constant"), lags = c("short")) 

pperron21 =ur.pp(y2, type=c("Z-alpha"), model = c("constant"), lags = c("short")) 


## ----include=FALSE-------------------------------------------------------

ers1 = ur.ers(y1, type = c("P-test"), model="trend", lag.max = 52)

ers2 = ur.ers(y2, type = c("P-test"), model="trend", lag.max = 52)

ers11 = ur.ers(y1, type = c("P-test"), model="constant", lag.max = 52)

ers21 = ur.ers(y2, type = c("P-test"), model="constant", lag.max = 52)

## ----include=FALSE-------------------------------------------------------
ers_1 = ur.ers(y1, type = c("DF-GLS"), model="trend", lag.max = 5)

ers_2 = ur.ers(y2, type = c("DF-GLS"), model="trend", lag.max = 5)

ers_11 = ur.ers(y1, type = c("DF-GLS"), model="constant", lag.max = 5)

ers_21 = ur.ers(y2, type = c("DF-GLS"), model="constant", lag.max = 5)


## ----include=FALSE-------------------------------------------------------
kpss1 = ur.kpss(y1, type=c("tau"), lags=c("short"))

kpss2 = ur.kpss(y2, type=c("tau"), lags=c("short"))

kpss11 = ur.kpss(y1, type=c("mu"), lags=c("short"))

kpss21 = ur.kpss(y2, type=c("mu"), lags=c("short"))

## ----echo=FALSE, include=FALSE-------------------------------------------
if (!require(urca)) install.packages("urca"); require(urca)

dickey11 = ur.df(diff(y1), type="none", selectlags = c("BIC"))
dickey21 = ur.df(diff(y2), type="none", selectlags = c("BIC"))
summary(dickey11)
summary(dickey21)


## ----include=FALSE-------------------------------------------------------
kpss1 = ur.kpss(diff(y1), type=c("mu"), lags=c("short"))
summary(kpss1)
kpss2 = ur.kpss(diff(y2), type=c("mu"), lags=c("short"))
summary(kpss2)

## ----include=FALSE-------------------------------------------------------
pperron1 =ur.pp(diff(y1), type=c("Z-alpha"), model = c("constant"), lags = c("short")) 
summary(pperron1)
pperron2 =ur.pp(diff(y2), type=c("Z-alpha"), model = c("constant"), lags = c("short")) 
summary(pperron2)

## ----include=FALSE-------------------------------------------------------
pperron11 =ur.pp(y1, type=c("Z-alpha"), model = c("constant"), lags = c("short")) 

pperron21 =ur.pp(y2, type=c("Z-alpha"), model = c("constant"), lags = c("short")) 


## ----include=FALSE-------------------------------------------------------

ers1 = ur.ers(y1, type = c("P-test"), model="trend", lag.max = 52)

ers2 = ur.ers(y2, type = c("P-test"), model="trend", lag.max = 52)

ers11 = ur.ers(y1, type = c("P-test"), model="constant", lag.max = 52)

ers21 = ur.ers(y2, type = c("P-test"), model="constant", lag.max = 52)

## ----include=FALSE-------------------------------------------------------
ers_1 = ur.ers(y1, type = c("DF-GLS"), model="trend", lag.max = 5)

ers_2 = ur.ers(y2, type = c("DF-GLS"), model="trend", lag.max = 5)

ers_11 = ur.ers(y1, type = c("DF-GLS"), model="constant", lag.max = 5)

ers_21 = ur.ers(y2, type = c("DF-GLS"), model="constant", lag.max = 5)
