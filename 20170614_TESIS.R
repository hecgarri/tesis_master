## ----include=FALSE-------------------------------------------------------
knitr::opts_chunk$set(echo = FALSE, warning = FALSE, out.width='4.5in', out.height='3.5in', fig.align='center', message=FALSE, fig.pos='H')

## ----echo=FALSE, results='hide', message=FALSE, warning=FALSE------------
setwd("/home/hector/GoogleDrivePersonal/Master/Tesis/GitHub/tesis_master")
source("weekly_palta.R")
#setwd("/home/hector/GoogleDrivePersonal/Master/Tesis/GitHub/tesis_master")
#source("weekly_tomate.R")

## ----results='hide', echo=FALSE, message=FALSE---------------------------
if (!require(forecast)) install.packages("forecast")

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


## ----fig-1,echo=F,fig.cap='Evolución de precios del palta Hass de primera calidad,2008-2016', out.width='6.5in', out.height='3.5in', fig.align='center', fig.pos='H'----

par(family="serif", bty="l", bg="white", cex.lab=1) # opciones gráficas
ts.plot(exp(precio_mayorista), exp(precio_supermercado), lty=1:2,
        lwd=1, ylab="$/kilo", xlab="Tiempo")
legend("topleft", legend = c("Mayorista", "Supermercado"),lty=1:2, lwd=2, cex=0.8)


## ----fig-2,echo=F,fig.cap='Imputación de valores perdidos a través del filtro de Kalman', out.width='6.5in', out.height='4.5in', fig.align='center', fig.pos='H'----
layout(matrix(c(1,1,1,1,1,
                2,2,2,2,2),2,5, byrow=TRUE))

par(family="serif", bty="l", par(family="serif", bty="l")) # opciones gráficas
plot(precio_mayorista, lty=2 ,ylab="Precio palta mayorista ($/kilo)", 
     xlab="Tiempo")
lines(y1, lwd=1, lty=1)
points(time(y1)[id.na1], y1[id.na1], col = "red", pch = 18, cex=2)
legend("topleft", legend = c("Valores imputados"), 
  col = c("red"), pch = c(18), cex=1.5)

plot(precio_supermercado, lty=2 ,ylab="Precio palta supermercado ($/kilo)", 
     xlab="Tiempo")
lines(y2, lwd=1, lty=1)
points(time(y2)[id.na2], y2[id.na2], col = "blue", pch = 18, cex=2)
legend("topleft", legend = c("Valores imputados"), 
  col = c("blue"), pch = c(18), cex=1.5)


## ----fig-2.1,echo=F,fig.cap='Evolución del logaritmo del precio mayorista de la palta, 2008-2016\\label{fig5.1}', out.width='6in', out.height='6.5in', fig.align='center'----
par(family="serif", bty="l")
layout(matrix(c(1,1,1,1,1,
                2,2,0,3,3),ncol = 5, byrow=TRUE))
plot(y1, main="a) Evolución log(precios) mayoristas, 2008-2016", 
     ylab="$/kilo")
Acf(y1, main="b) Función de autocorrelación", lag.max = 52)
Pacf(y1, main="c) Función de autocorrelación parcial", lag.max = 52)

## ----fig-2.2,echo=F,fig.cap='Evolución del logaritmo del precio en supermercado de la palta, 2008-2016\\label{fig5.2}', out.width='6in', out.height='6.5in', fig.align='center'----
par(family="serif", bty="l")
layout(matrix(c(1,1,1,1,1,
                2,2,0,3,3),ncol =5, byrow=TRUE))
plot(y2, main="a) Evolución log(precios) supermercado, 2008-2016", 
     ylab="$/kilo")
Acf(y2, main="b) Función de autocorrelación", lag.max = 52)
Pacf(y2, main="c) Función de autocorrelación parcial", lag.max = 52)

## ----echo=FALSE, include=FALSE-------------------------------------------
if (!require(urca)) install.packages("urca")

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

## ----echo=FALSE----------------------------------------------------------
if (!require(vars)) install.packages("vars"); require(vars)
if (!require(tsDyn)) install.packages("tsDyn"); require(vars)
if (!require(ggplot2)) installed.packages("ggplot2")
if (!require("reshape2")) install.packages("reshape2")
if (!require(RColorBrewer)) install.packages("RColorBrewer")

datos = data.frame(y2,y1)
names(datos) = c("supermercado", "mayorista")

## ----out.width='5in', out.height='3.5in',fig.cap="Inclusión de variables escalón"----
# Crearé una variable dummy que recoja el efecto del pulso de 2014 en la serie. 

dummy1 = ifelse(row(datos)[,1]>=312,1,0) 
dummy2 = ifelse(datos[,1]<7,row(datos),0) 

dummy = cbind(dummy1, dummy2)
colnames(dummy) = c("salto_1", "salto_2")

layout(matrix(c(1,1,1,1,1,
                2,2,2,2,2),2,5, byrow=TRUE))

data = cbind(row(datos)[,1], datos[,1])
plot(data[,2]~data[,1], type="l", ylab="log(precio)")
bajo = mean(datos[76:121,1])
medio = mean(datos[122:328,1])
alto = mean(datos[329:494,1])
segments(c(1,76,122,329),c(medio,bajo,medio,alto),
         c(75,121,328,494),c(medio,bajo,medio,alto),
         lwd=3, col = "red", lty =1)
data = cbind(row(datos)[,2], datos[,2])
plot(data[,2]~data[,1], type="l", ylab="log(precio)")
bajo = mean(datos[76:121,2])
medio = mean(datos[122:328,2])
alto = mean(datos[329:494,2])
segments(c(1,76,122,329),c(medio,bajo,medio,alto),
         c(75,121,328,494),c(medio,bajo,medio,alto),
         lwd=3, col = "red", lty =1)

## ----out.width='5in', out.height='3.5in',fig.cap="Número de Rezagos para el contraste de Independencia"----
#Para ver las fechas 
fechas = data.frame(as.yearmon(time(datos[,1])))

vecm2 = lapply(2:8, function(x) ca.jo(datos, type=c("trace"), ecdet=c( "const"), K=x, spec="transitory", season=NULL, dumvar = dummy))

modelo = lapply(1:7, function(x) vec2var(vecm2[[x]], r=1))


lags = 3:52
resultado = sapply(lags, function(x) lapply(1:length(modelo), 
                        function(y) serial.test( modelo[[y]],
                       lags.pt = x)$serial$p.value) %>% as.numeric()) %>% t() %>% 
  data.frame() %>% mutate(rezago = lags)


meltresultado = melt(resultado, id = "rezago", na.rm=TRUE) %>% 
  rename(Modelo = variable, `p-value`=value) 

meltresultado$Modelo = recode(meltresultado$Modelo,
                              'X1'='2 rezagos',
                              'X2'='3 rezagos',
                              'X3'='4 rezagos',
                              'X4'='5 rezagos',
                              'X5'='6 rezagos',
                              'X6' = '7 rezagos',
                              'X7' = '8 rezagos')

ggplot(meltresultado, aes(x=rezago, y=`p-value`, colour=Modelo, group=Modelo)) + 
  geom_line(size=1.2)+geom_hline(yintercept = 0.05, color="red")+
  geom_hline(yintercept = 0.1, col="red2")+
  theme(panel.background = element_rect(),
        plot.background = element_rect(colour = "white",size = 0.5), 
        axis.text.x = element_text(size=10, family="serif"), 
        axis.title.x = element_text(size=15, family="serif"), 
        axis.title.y = element_text(size=15, family = "serif"), 
        legend.background = element_rect(fill="grey95"), 
        legend.text = element_text(size=10, family="serif"), 
        legend.title = element_text(face="bold", family="serif"), 
        legend.title.align = 0.5)+
#  scale_colour_manual(values = brewer.pal(12,"Blues"))+
  scale_x_continuous(breaks = c(5,10,15,20,25,30))+
  scale_y_continuous(breaks = seq(0.0,1,0.05))

## ----include = FALSE, echo=FALSE-----------------------------------------
## Pruebo dos formulaciones equivalentes 
modelo_alternativo = VECM(datos, lag=7, r=1, LRinclude= c("both"), 
              estim = c("ML"), exogen = dummy)

## Pruebo dos formulaciones equivalentes 
modelo_nulo = VECM(datos, lag=7, r=1, LRinclude= c("trend"), 
              estim = c("ML"), exogen = dummy)

D = 2*(logLik(modelo_nulo)-logLik(modelo_alternativo))

beta = c(0,0)

modelo_ = ca.jo(datos, type = c("trace"), ecdet = c("const"), K=8,
               spec = c("transitory"), dumvar = dummy)

modelo_1 = ca.jo(datos, type = c("eigen"), ecdet = c("const"), K=8,
               spec = c("transitory"))

modelo__ = cajorls(modelo_, r=1)


## ----echo=FALSE----------------------------------------------------------
#source("girf_sample.R")

## ----include = FALSE-----------------------------------------------------
VARselect(datos, type = c("const"), exogen = dummy)$selection

modelito = VAR(datos,p=8, type=c("const"), exogen =dummy)

## ----include = FALSE-----------------------------------------------------
if (!require(stargazer)) install.packages("stargazer"); require(stargazer)
if (!require(xtable)) install.packages("xtable"); require(xtable)

print(xtable(summary(modelito)$varresult$supermercado, digits = 2), comment = FALSE)

print(xtable(summary(modelito)$varresult$mayorista, digits = 2), comment = FALSE)


## ------------------------------------------------------------------------
impulso = irf(modelito,n.ahead = 78, cumulative=FALSE)
plot(impulso)

## ----include = FALSE, echo = FALSE---------------------------------------

if (!require(asbio)) install.packages("asbio")

errores = data.frame(residuals(modelito))
DH.test(errores, names(errores))

## ----echo=FALSE----------------------------------------------------------
res = residuals(modelito)
if (!require(plot3D)) install.packages("plot3D")

x = res[,c("mayorista")]
y = res[,c("supermercado")]

x_c = cut(x,20)
y_c = cut(y,20)

z = table(x_c,y_c)

layout(matrix(c(1,1,1,1,1,
                2,2,2,2,2),2,5, byrow=TRUE))

hist3D(z=z, border="black", contour=TRUE)
image2D(z=z, border="black")

## ----fig-5,echo=F,fig.cap='Modelo de corrección del error por umbrales', out.width='4.5in', out.height='4.5in', fig.align='center', message=FALSE, fig.pos='H'----

#if (!require(tsDyn)) install.packages("tsDyn")

#mono = TVECM(datos, lag=2, nthresh = 1, trim=0.05, ngridBeta = 100, ngridTh = 500, plot=TRUE, include=c("const"))

#Hansen = TVECM.HStest(datos, lag=3, ngridTh = 300, trim=0.05, nboot=100)

## ----echo=FALSE----------------------------------------------------------
normalidad = function (x, multivariate.only = TRUE) 
{
    if (!((class(x) == "TVECM") || (class(x) == "nlvar"))) {
        stop("\nPlease provide an object of class 'varest', generated by 'var()', or an object of class 'vec2var' generated by 'vec2var()'.\n")
    }
    obj.name <- deparse(substitute(x))
    K <- x$k
    # cambio de la forma de recoger el número de observaciones
    obs <- x$t 
    resid <- resid(x)
    resids <- scale(resid, scale = FALSE)
    # llamo directamente a la función .jb.multi desde el paquete vars
    jbm.resids <- vars:::.jb.multi(resids, obs = obs, K = K, obj.name = obj.name)
    if (multivariate.only) {
        result <- list(resid = resid, jb.mul = jbm.resids)
    }
    else {
        jbu.resids <- apply(resids, 2, function(x) .jb.uni(x, 
            obs = obs))
        for (i in 1:K) jbu.resids[[i]][5] <- paste("Residual of", 
            colnames(resids)[i], "equation")
        result <- list(resid = resid, jb.uni = jbu.resids, jb.mul = jbm.resids)
    }
    class(result) <- "varcheck"
    return(result)
}


## ------------------------------------------------------------------------
lrt <- function (obj1, obj2, r) {
   L0 <- logLik(obj1, r=r)
   L1 <- logLik(obj2, r=r)
   L01 <- as.vector(- 2 * (L0 - L1))
   df <- obj1@P^2
   list(L01 = L01, df = df,
       "p-value" = pchisq(L01, df, lower.tail = FALSE))
}

lrt(vecm1, vecm1.1, r=1)


