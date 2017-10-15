## ----include=FALSE-------------------------------------------------------
produ = c(8190,15050,	17047,	18463,	20181,	21208,	22290,	23260,	23800,	24000,	26731,	26744,	26759,	33837,	33531,	34057,	36388,	35679,	36355,	31727,	29908,	29933) 

## ------------------------------------------------------------------------
ts.plot(produ)

## ----include=FALSE-------------------------------------------------------
knitr::opts_chunk$set(echo = FALSE, warning = FALSE, out.width='4.5in', out.height='3.5in', fig.align='center', message=FALSE, fig.pos='H')

## ----echo=FALSE, results='hide', message=FALSE, warning=FALSE------------
setwd("/home/hector/GoogleDrivePersonal/Master/Tesis/GitHub/tesis_master")
#source("importa_semanales.R")
source("weekly_palta.R")

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


## ----fig-1,echo=F,fig.cap='Evolución de precios del palta de larga vida de primera calidad,2008-2016', out.width='6.5in', out.height='3.5in', fig.align='center', fig.pos='H'----

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


## ----fig-2.1,echo=F,fig.cap='Evolución del logaritmo del precio mayorista de la palta ,2008-2016', out.width='6in', out.height='6.5in', fig.align='center', fig.pos='H'----
par(family="serif", bty="l")
layout(matrix(c(1,1,1,1,1,
                2,2,0,3,3,
                4,4,4,4,4),3,5, byrow=TRUE))
plot(y1, main="a) Evolución log(precios) mayoristas, 2008-2016", 
     ylab="$/kilo")
Acf(y1, main="b) Función de autocorrelación", lag.max = 200)
Pacf(y1, main="c) Función de autocorrelación parcial", lag.max = 200)
boxplot(y1~cycle(y1), main="Diagrama de caja y bigote")

## ----fig-2.2,echo=F,fig.cap='Evolución del logaritmo del precio en supermercado de la palta ,2008-2016', out.width='6in', out.height='6.5in', fig.align='center', fig.pos='H'----
par(family="serif", bty="l")
layout(matrix(c(1,1,1,1,1,
                2,2,0,3,3,
                4,4,4,4,4),3,5, byrow=TRUE))
plot(y2, main="a) Evolución log(precios) supermercado, 2008-2016", 
     ylab="$/kilo")
Acf(y2, main="b) Función de autocorrelación", lag.max = 200)
Pacf(y2, main="c) Función de autocorrelación parcial", lag.max = 200)
boxplot(y2~cycle(y2), main="Diagrama de caja y bigote")

## ----echo=FALSE, include=FALSE-------------------------------------------
if (!require(urca)) install.packages("urca")

dickey1 = ur.df(y1, type="trend", selectlags = c("BIC"))
summary(dickey1)
dickey2 = ur.df(y2, type="trend", selectlags = c("BIC"))
summary(dickey2)

dickey11 = ur.df(y1, type="drift", selectlags = c("BIC"))
dickey21 = ur.df(y2, type="drift", selectlags = c("BIC"))
summary(dickey11)
summary(dickey21)

## ----include=FALSE-------------------------------------------------------
pperron1 =ur.pp(y1, type=c("Z-alpha"), model = c("trend"), lags = c("short")) 

pperron2 =ur.pp(y2, type=c("Z-alpha"), model = c("trend"), lags = c("short")) 


## ----include=FALSE-------------------------------------------------------
pperron11 =ur.pp(y1, type=c("Z-alpha"), model = c("constant"), lags = c("short")) 

pperron21 =ur.pp(y2, type=c("Z-alpha"), model = c("constant"), lags = c("short")) 


## ----include=FALSE-------------------------------------------------------

ers1 = ur.ers(y1, type = c("P-test"), model="trend", lag.max = 1)

ers2 = ur.ers(y2, type = c("P-test"), model="trend", lag.max = 1)

ers11 = ur.ers(y1, type = c("P-test"), model="constant", lag.max = 1)

ers21 = ur.ers(y2, type = c("P-test"), model="constant", lag.max = 1)

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

## ----include = FALSE-----------------------------------------------------
if (!require(uroot)) install.packages("uroot")

#ch.test(y1)

## ----echo=FALSE----------------------------------------------------------
datos = data.frame(y2,y1)
names(datos) = c("supermercado","mayorista")

## ----echo=FALSE, size = "scriptsize", message=FALSE----------------------

if (!require(vars)) install.packages("vars")
if (!require(tsDyn)) install.packages("tsDyn")

### Para seleccionar el número de rezagos del modelo utilizaré un contraste de razón de verosimilitud

dummy = ifelse(row(datos)>312,1,0)

VARselect(datos, lag.max = 52, type = "const", exogen = dummy[,1])


vecm1 = ca.jo(datos, type=c("trace"), ecdet=c("none"), K=8,
              spec="transitory", season=NULL, dumvar = dummy[,1])

var_model  = VAR(datos,p=8, type="const", exogen = dummy[,1])
var_model2  = VAR(datos,p=8, type="const")


lrt <- function (obj1, obj2, r) {
  L0 <- logLik(obj1)
  L1 <- logLik(obj2)
  L01 <- as.vector(- 2 * (L0 - L1))
  df <- 1
  list(L01 = L01, df = df,
       "p-value" = pchisq(L01, df, lower.tail = FALSE))
}

lrt(var_model2,var_model)


normality(var_model)
arch.test(var_model)
lapply(8:30, function(i) serial.test(var_model,
             lags.pt = i, type="PT.asymptotic")$serial$p.value) %>% as.numeric()

plot(stability(var_model, type="OLS-CUSUM"))
plot(stability(var_model, type="Rec-CUSUM"))
plot(stability(var_model, type="Rec-MOSUM"))

plot(irf(var_model, n.ahead = 52))

vecm1.1 = ca.jo(datos, type=c("trace"), ecdet=c("const"), K=20,
              spec="longrun", season=NULL)

lrt <- function (obj1, obj2, r) {
   L0 <- logLik(obj1, r=r)
   L1 <- logLik(obj2, r=r)
   L01 <- as.vector(- 2 * (L0 - L1))
   df <- obj1@P^2
   list(L01 = L01, df = df,
       "p-value" = pchisq(L01, df, lower.tail = FALSE))
}

lrt(vecm1, vecm1.1, r=1)


 

## ----out.width='5in', out.height='3.5in',fig.cap="Número de Rezagos para el contraste de Independencia"----
if (!require(ggplot2)) installed.packages("ggplot2")
if (!require("reshape2")) install.packages("reshape2")
if (!require(RColorBrewer)) install.packages("RColorBrewer")



beta = cbind(mayorista = 1, supermercado=1, const = 1)

modelo1 = VECM(datos, r=1, lag = 2, LRinclude=c("const"), estim=c("ML"))

## Recordar que esta secuencia se realiza para imponer restricciones sobre 
# la estimación del modelo 

vecm1 = ca.jo(datos, type=c("trace"), ecdet=c("const"), K=3,
              spec="longrun", season=NULL)

plot(vecm1)

## ------------------------------------------------------------------------
HD0 = c(1,-1.06,0.98)

modelo11 = blrtest(vecm1, HD0, r=1)
summary(modelo11)

modelo12 = cajorls(modelo11, r=1)

summary(modelo1)

vecm2 = lapply(2:30, function(x) ca.jo(datos, type=c("eigen"), ecdet=c( "const"), K=x,
                                      spec="longrun", season=NULL))

modelo = lapply(1:29, function(x) vec2var(vecm2[[x]], r=1))


lags = 3:30
resultado = sapply(lags, function(x) lapply(1:length(modelo), 
                        function(y) serial.test( modelo[[y]],
                       lags.pt = x)$serial$p.value) %>% as.numeric()) %>% t() %>% 
  data.frame() %>% mutate(rezago = lags)


meltresultado = melt(resultado, id = "rezago", na.rm=TRUE) %>% 
  rename(Modelo = variable, `p-value`=value) 

meltresultado$Modelo = recode(meltresultado$Modelo, 'X1'='1 rezago', 'X2'='2 rezagos', 
       'X3'='3 rezagos', 'X4'='4 rezagos', 'X5'='5 rezagos', 
       'X6' = '6 rezagos')

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
  scale_colour_manual(values = brewer.pal(6,"Blues"))+
  scale_x_continuous(breaks = c(5,10,15,20,25,30))+
  scale_y_continuous(breaks = seq(0.0,1,0.05))

## ------------------------------------------------------------------------
modelo = VECM(datos, lag=7, r=1, LRinclude= c("const"), 
              estim = c("ML"))

summary(modelo)

## ----echo=FALSE----------------------------------------------------------
#source("girf_sample.R")

## ----fig-5,echo=F,fig.cap='Modelo de corrección del error por umbrales', out.width='4.5in', out.height='4.5in', fig.align='center', message=FALSE, fig.pos='H'----

if (!require(tsDyn)) install.packages("tsDyn")

mono = TVECM(datos, lag=7, nthresh = 1, trim=0.05,
       ngridBeta = 100, ngridTh = 100, plot=TRUE,
       include=c("const"), beta0 = rep(1, length(datos$mayorista)))

Hansen = TVECM.HStest(datos, lag=7, ngridTh = 300, trim=0.05, nboot=100)

## ----results='asis'------------------------------------------------------
toLatex(mono)

## ------------------------------------------------------------------------
plot(Hansen)

summary(mono)

## ----echo=FALSE----------------------------------------------------------
res = residuals(mono)
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

normalidad(mono)

if (!require(asbio)) install.packages("asbio")

errores = data.frame(residuals(mono))
DH.test(errores, names(errores))

