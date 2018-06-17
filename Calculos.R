## ----include=FALSE-------------------------------------------------------
knitr::opts_chunk$set(echo = FALSE, warning = FALSE, out.width='4.5in', out.height='3.5in', fig.align='center', message=FALSE, fig.pos='H')

## ----echo=FALSE, results='hide', message=FALSE, warning=FALSE------------
if (!require(tidyverse)) install.packages("tidyverse"); require(tidyverse)
if (!require(readxl)) install.packages("readxl"); require(readxl)
if (!require(readr)) install.packages("readr"); require(readr)
if (!require(RColorBrewer)) install.packages("RColorBrewer"); require(RColorBrewer)


setwd("/home/hector/GoogleDrivePersonal/Master/Tesis/GitHub/tesis_master")
source("weekly_palta.R")
#setwd("/home/hector/GoogleDrivePersonal/Master/Tesis/GitHub/tesis_master")
#source("weekly_tomate.R")

## ----fig-1,echo=F,fig.cap='Evolución de precios del palta Hass de primera calidad,2008-2017.', out.width='6.5in', out.height='3.5in', fig.align='center', fig.pos='H'----

par(family="serif", bty="l", bg="white", cex.lab=1) # opciones gráficas

precios = left_join(price_may_palta, price_sup_palta, by = "start") %>%
  select(year.x,month.x,day.x,price.x, price.y) %>% 
  melt(id = c("year.x","month.x","day.x")) %>% mutate(precio = exp(value)) %>% 
  mutate(fecha = as.Date(paste0(year.x,"/",month.x,"/",day.x)))

ggplot(precios,aes(x=fecha,y=precio,colour=variable,group=variable)) + geom_line()+
  scale_colour_discrete(name  ="Precio",
                          breaks=c("price.x", "price.y"),
                          labels=c("Mayorista", "Supermercado"))+ylab("Precio ($/kilo)")

## ----include=FALSE-------------------------------------------------------
arribo_mayoristas <- read_excel("~/GoogleDrivePersonal/Master/Tesis/datos/Volumen_Paltas.xls", range = "B6:O50") 

arribo_mayoristas = arribo_mayoristas %>% select(-Total) %>% 
  gather(mes, volumen,-Años) %>% arrange(Años) %>% filter(Años!=2018) %>% 
  mutate(fecha = seq(as.Date("1975/01/01"),as.Date("2017/12/01"),by="month")) %>% 
  select(-c(Años,mes)) 

## ----fig.cap="Volúmenes de palta arribados en mercados mayoristas 1975-2017\\label{volumen}",out.width='4.5in', out.height='3.5in', fig.align='center', message=FALSE----
options(scipen = 10)
ggplot(arribo_mayoristas[1:516,], aes(fecha, volumen/1000000)) + geom_line() +
   xlab("") + ylab("Miles de toneladas")

## ----echo=FALSE,fig.cap="Superficie plantada de Paltos\\label{Paltos}",out.width='4.5in', out.height='3.5in', fig.align='center', message=FALSE----
catastro <- read_excel("~/GoogleDrivePersonal/Master/Tesis/datos/Datos-Catastro_may.xlsx")

catastro = catastro %>% filter(Especie=="Palto") %>% select(-Especie) %>% 
  rename(superficie = `Superficie comercial (hectáreas)`)

ggplot(catastro,aes(x=Año,y=superficie,colour=Región,group=Región)) + geom_line()+
  scale_colour_manual(values = brewer.pal(10,"Set3"))

## ----echo = FALSE--------------------------------------------------------
if (!require(survey)) install.packages("survey"); require(survey)
if (!require(haven)) install.packages("haven"); require(haven)


gasto <- read_dta("~/GoogleDrivePersonal/Master/Tesis/datos/BASE_GASTOS_VIIEPF.dta")
gasto = gasto  %>% mutate(PERSONA = 1) %>% arrange(FOLIO, PERSONA)


glosario_epf <- read_excel("~/GoogleDrivePersonal/Master/Tesis/datos/glosario_epf.xlsx", 
     range = "A2:D87")

clasificacion <- read_excel("~/GoogleDrivePersonal/Master/Tesis/datos/clasificacion_de_consumo_individual_por_finalidades.xls", range = "A3:H1573")

legumbres_hortalizas = clasificacion %>% filter(D==0,X__1==1,G==1, C==7)

gasto = gasto %>% mutate(legumbres = grepl("^01.1.7",CCIF)) %>% 
  filter(legumbres == 1,P!=0) 

design = svydesign(~1, weights = ~FE, data = gasto)

consumo_promedio = svyby(~GASTO, by = ~GLOSA, design = design, svymean) %>% arrange(desc(GASTO))

rownames(consumo_promedio) = rownames(consumo_promedio) %>% as.numeric()

otras = consumo_promedio$GLOSA[12:50]

design$variables = design$variables %>% mutate(glosa = ifelse(GLOSA %in% otras,"Otras",GLOSA))

consumo_promedio = svyby(~GASTO, by = ~glosa, design = design, svymean) %>% arrange(desc(GASTO)) %>% 
  mutate(glosa = tolower(glosa))



## ----results = 'asis'----------------------------------------------------
xtable::xtable(consumo_promedio[,c(1,2)], type = "latex", caption = "Gasto promedio en legumbres y hortalizas en Chile",
               digits = 1)

## ------------------------------------------------------------------------
world_prod <- read_csv("~/GoogleDrivePersonal/Master/Tesis/datos/Producción mundial de palta.csv")

produccion = world_prod %>% filter(Element == "Production")

produccion_chile = world_prod %>% filter(Element == "Production", Area =="Chile")

## ----fig-2,echo=F,fig.cap='Evolución de precios del palta Hass de primera calidad,2008-2016', out.width='6.5in', out.height='3.5in', fig.align='center', fig.pos='H'----

par(family="serif", bty="l", bg="white", cex.lab=1) # opciones gráficas
ts.plot(precio_mayorista, precio_supermercado, lty=1:2,
        lwd=1, ylab="$/kilo", xlab="Tiempo")
legend("topleft", legend = c("Mayorista", "Supermercado"),lty=1:2, lwd=2, cex=0.8)


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


## ----echo=FALSE----------------------------------------------------------
if (!require(vars)) install.packages("vars"); require(vars)
if (!require(tsDyn)) install.packages("tsDyn"); require(vars)
if (!require(ggplot2)) installed.packages("ggplot2"); require(ggplot2)
if (!require("reshape2")) install.packages("reshape2"); require(reshape2)

datos = data.frame(y1,y2)
names(datos) = c("mayorista", "supermercado")

## ----out.width='5in', out.height='3.5in',fig.cap="Número de Rezagos para el contraste de Independencia\\label{fig5.6}", fig.pos='!htpb'----
#Para ver las fechas 
fechas = data.frame(as.yearmon(time(datos[,1])))

vecm2 = lapply(2:8, function(x) ca.jo(datos, type=c("trace"), ecdet=c( "const"), K=x, spec="transitory", season=NULL))

modelo = lapply(1:7, function(x) vec2var(vecm2[[x]], r=1))

lags = 3:52
resultado = sapply(lags, function(x) lapply(1:length(modelo), 
                        function(y) serial.test( modelo[[y]],
                       lags.pt = x)$serial$p.value) %>% as.numeric()) %>% t() %>% 
  data.frame() %>% mutate(rezago = lags)

meltresultado = melt(resultado, id = "rezago", na.rm=TRUE) %>% 
  rename(Modelo = variable, `p-value`=value) 

meltresultado$Modelo = recode(meltresultado$Modelo,
                              'X1'='1 rezagos',
                              'X2'='2 rezagos',
                              'X3'='3 rezagos',
                              'X4'='4 rezagos',
                              'X5'='5 rezagos',
                              'X6' = '6 rezagos',
                              'X7' = '7 rezagos')

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
  scale_x_continuous(breaks = c(5,10,15,20,25,30,35,40,45,50))+
  scale_y_continuous(breaks = seq(0.0,1,0.05))

## ----include = FALSE, echo=FALSE-----------------------------------------
## Pruebo dos formulaciones equivalentes 
modelo_alternativo = VECM(datos[,c(2,1)], lag=7, r=1, LRinclude= c("const"), 
              estim = c("ML"))

modelo_ = ca.jo(datos[,c(2,1)], type = c("trace"), ecdet = c("const"), K=8,
               spec = c("transitory"))

modelo1_ = ca.jo(datos[,c(2,1)], type = c("eigen"), ecdet = c("const"), K=8,
               spec = c("transitory"))

modelo__=cajorls(modelo_,r=1)

## ------------------------------------------------------------------------
not_restrict = VECM(datos, lag=7, estim="ML", r=1, LRinclude = c("const"))

modelo_ = ca.jo(datos, type = c("trace"), ecdet = c("const"), K=8,
               spec = c("transitory"))
H0=matrix(c(1,-1,0,0,0,1),c(3,2))
homogeneity = blrtest(modelo_,H=H0,r=1)

## ------------------------------------------------------------------------
restrict = cajorls(homogeneity,r=1)$rlm

## ------------------------------------------------------------------------
homogeneity = blrtest(modelo_,H=H0,r=1)

weak_exogeneity = alrtest(modelo_,A=c(1,0),r=1)

## ----echo = F------------------------------------------------------------
var_version = vec2var(modelo_, r= 1)

impulso = irf(var_version, n.ahead = 52)
impulso2 = irf(var_version, n.ahead = 52, ortho = FALSE)

## ----graph5.6, echo=FALSE, fig.cap='Función ortogonal de impulso respuesta\\label{graph5.6}', fig.pos='!htpb'----
source("plot.varirf.R")
plot.varirf1(impulso)

## ----graph5.7, echo=FALSE, fig.cap='Función acumulada de impulso respuesta\\label{graph5.7}', fig.pos='!htpb'----

plot.varirf1(impulso2)

## ----graph5.8, echo=FALSE, fig.cap='Residuos del modelo\\label{graph5.8}', fig.pos='!htpb'----
layout(matrix(c(1,1,1,1,1,
                2,2,2,2,2,
                3,3,0,4,4),3,5, byrow=TRUE))
plot(ts(residuals(var_version)[,1]), main = "Residuos de la ecuación mayorista")
plot(ts(residuals(var_version)[,1]), main = "Residuos de la ecuación supermercado")
Acf(residuals(var_version)[,1], main = "ACF mayorista")
Acf(residuals(var_version)[,2], main = "ACF supermercado")

## ----echo=FALSE----------------------------------------------------------
res = residuals(var_version)
if (!require(plot3D)) install.packages("plot3D")

x = res[,1]
y = res[,2]

x_c = cut(x,20)
y_c = cut(y,20)

z = table(x_c,y_c)

layout(matrix(c(1,1,1,1,1,
                2,2,2,2,2),2,5, byrow=TRUE))


## ----fig-5.3.1,echo=F,fig.cap='Histograma bivariado de los residuos del modelo', out.width='3.5in', out.height='3.5in', fig.align='center', message=FALSE, fig.pos='!htpb'----
hist3D(z=z, border="black", contour=TRUE)


## ----fig-5.3.2,echo=F,fig.cap='Mapa de calor de los residuos del modelo', out.width='3.5in', out.height='3.5in', fig.align='center', message=FALSE, fig.pos='!htpb'----
image2D(z=z, border="black")

## ----include = FALSE, echo = FALSE---------------------------------------

if (!require(asbio)) install.packages("asbio")

errores = data.frame(residuals(var_version))
DH.test(errores, names(errores))
normality.test(var_version)
arch.test(var_version, lags.multi = 9)

## ------------------------------------------------------------------------
TVECM_rest = function (data, lag = 1, ngridTh = 300, trim = 0.05, nboot = 100, 
          fixed.beta = NULL, intercept = TRUE, boot.type = c("FixedReg", 
                                                             "ResBoot"), hpc = c("none", "foreach")) 
{
  boot.type <- match.arg(boot.type)
  hpc <- match.arg(hpc)
  dir = FALSE
  data <- as.matrix(data)
  if (ncol(data) > 2) {
    warning("Please no more than two equations")
  }
  if (is.null(colnames(data))) {
    colnames(data) <- paste("Var", c(1:2), sep = "")
  }
  T <- nrow(data)
  p <- lag
  y <- diff(data)[(p + 1):(T - 1), ]
  DeltaX <- embed(diff(data), p + 1)[, -(1:2)]
  if (intercept) 
    DeltaX <- cbind(1, DeltaX)
  x <- DeltaX
  t <- nrow(y)
  if (is.null(fixed.beta)) {
    ve <- VECM(data, lag = lag, LRinclude = "const", estim = "ML")
  }
  else {
    ve <- VECM(data, lag = lag, LRinclude = "const", beta = fixed.beta, 
               estim = "2OLS")
  }
  beta <- ve$model.specific$coint[2, 1]
  ect <- ve$model[, grep("ECT", colnames(ve$model))]
  w0 <- matrix(ect[!is.na(ect)], ncol = 1)
  if (FALSE) {
    b_like <- function(b) {
      z <- cbind(xlag %*% c(1, -b), x)
      sigma <- crossprod(residuals(lm.fit(z, y)))/t
      like <- (t/2) * log(det(sigma))
      like
    }
    nlike1 <- b_like(b0 + 0.001)
    nlike2 <- b_like(b0 - 0.001)
    hp <- (nlike1 + nlike2 - 2 * nlike)/(0.001^2)
    seb <- 1/sqrt(hp)
  }
  q <- sort(w0)
  if (ngridTh > (1 - 2 * trim) * T) {
    ngridTh <- round((1 - 2 * trim) * T - 1)
    warning("ngridTh bigger than number of potential threshold values, set to ", 
            ngridTh, "\n")
  }
  gamma2 <- q[round(seq(from = trim * T, to = (1 - trim) * 
                          T, length.out = ngridTh))]
  gamma2 <- unique(gamma2)
  gammas_range <- range(q, na.rm = TRUE)
  ngridTh <- length(gamma2)
  lmtest02 <- function(y, x, w0, gammas, dir = dir) {
    X <- cbind(w0, x)
    if (dir) {
      q <- qr(X)
      res_restr <- qr.resid(q, y)
    }
    else {
      z0zz <- X %*% solve(t(X) %*% X)
      res_restr <- lm.fit(X, y)$residuals
    }
    res_restr1 <- res_restr[, 1]
    res_restr2 <- res_restr[, 2]
    store <- rep(NA, ngridTh)
    Ttrim <- trim * t
    ngridTh <- min(t * (1 - 2 * trim), length(gammas))
    for (j in 1:ngridTh) {
      d1 <- ifelse(w0 <= gammas[j], 1, 0)
      n1 <- sum(d1)
      if (min(c(n1, (t - n1))) > Ttrim) {
        z1 <- c(d1) * X
        res_unrestr <- if (dir) 
          qr.resid(q, z1)
        else z1 - z0zz %*% (t(X) %*% z1)
        zea <- res_restr1 * res_unrestr
        zeb <- res_restr2 * res_unrestr
        ze <- cbind(zea, zeb)
        v <- crossprod(ze)
        z11y <- crossprod(res_unrestr, y)
        s <- matrix(c(z11y), ncol = 1)
        VV <- crossprod(v)
        VVinv <- try(solve(VV), silent = TRUE)
        if (inherits(VVinv, "try-error")) 
          VVinv <- MASS::ginv(VV)
        store[j] <- t(s) %*% VVinv %*% t(v) %*% s
      }
    }
    return(store)
  }
  lmtest02_boot <- function(y, x, w0, gammas, dir = dir) {
    X <- cbind(w0, x)
    if (dir) {
      res_restr <- qr.resid(q, y)
    }
    else {
      res_restr <- lm.fit(X, y)$residuals
    }
    res_restr1 <- res_restr[, 1]
    res_restr2 <- res_restr[, 2]
    store <- rep(0, ngridTh)
    ngridTh <- min(t * (1 - 2 * trim), length(gammas))
    for (j in 1:ngridTh) {
      d1 <- ifelse(w0 <= gammas[j], 1, 0)
      n1 <- sum(d1)
      if (min(c(n1, (t - n1))) > Ttrim) {
        z1 <- c(d1) * X
        res_unrestr <- if (dir) 
          qr.resid(q, z1)
        else z1 - z0zz %*% (t(X) %*% z1)
        zea <- res_restr1 * res_unrestr
        zeb <- res_restr2 * res_unrestr
        ze <- cbind(zea, zeb)
        v <- crossprod(ze)
        z11y <- crossprod(res_unrestr, y)
        s <- matrix(c(z11y), ncol = 1)
        VV <- crossprod(v)
        VVinv <- try(solve(VV), silent = TRUE)
        if (inherits(VVinv, "try-error")) 
          VVinv <- MASS::ginv(VV)
        store[j] <- t(s) %*% VVinv %*% t(v) %*% s
      }
    }
    lm01 <- max(store, na.rm = TRUE)
    lm01
  }
  lm01 <- lmtest02(y, x, w0, gamma2, dir = dir)
  teststat <- max(lm01, na.rm = TRUE)
  if (nboot == 0) {
    CriticalValBoot <- NULL
    PvalBoot <- NULL
    boots.reps <- NULL
    if (hpc == "foreach") 
      warning("hpc='foreach' used only when nboot>0\n")
  }
  else if (nboot > 0) {
    if (boot.type == "FixedReg") {
      X <- cbind(w0, x)
      Ttrim <- trim * t
      if (dir) {
        q <- qr(X)
      }
      else {
        z0zz <- X %*% solve(t(X) %*% X)
      }
      lmtest_withBoot <- function(e) {
        yr <- rnorm(n = t, 0, 1) * e
        return(lmtest02_boot(yr, x, w0, gamma2, dir = dir))
      }
      boots.reps <- if (hpc == "none") 
        replicate(nboot, lmtest_withBoot(e = residuals(ve)))
      else foreach(i = 1:nboot, .export = "lmtest_withBoot", 
                   .combine = "c") %dopar% lmtest_withBoot(e = residuals(ve))
    }
    else {
      lmtest_with_resBoot <- function(ve) {
        data.boot <- TVECM.sim(TVECMobject = ve, type = "boot")
        if (is.null(fixed.beta)) {
          ve.boot <- VECM(data.boot, lag = lag, LRinclude = "const", 
                          estim = "ML")
        }
        else {
          ve.boot <- VECM(data.boot, lag = lag, LRinclude = "none", 
                          beta = fixed.beta, estim = "2OLS")
        }
        ect.boot <- ve.boot$model[, "ECT"]
        which.nas <- 1:(p + 1)
        w0.boot <- matrix(ect.boot[-which.nas], ncol = 1)
        x.boot <- ve.boot$model[-which.nas, -c(1:3)]
        y.boot <- ve.boot$model[, c(1:2)]
        y.boot <- diff(y.boot)[(p + 1):(T - 1), ]
        w0.ord.boot <- sort(w0)
        gamma2.boot <- w0.ord.boot[round(seq(from = trim * 
                                               T, to = (1 - trim) * T, length.out = ngridTh))]
        gamma2.boot <- unique(gamma2.boot)
        ngridTh.boot <- length(gamma2.boot)
        test.boot <- lmtest02(y.boot, x.boot, w0.boot, 
                              gamma2.boot, dir = dir)
        return(max(test.boot, na.rm = TRUE))
      }
      boots.reps <- if (hpc == "none") 
        replicate(nboot, lmtest_with_resBoot(ve))
      else foreach(i = 1:nboot, .export = "lmtest_with_resBoot", 
                   .combine = "c") %dopar% lmtest_with_resBoot
    }
    PvalBoot <- mean(ifelse(boots.reps > teststat, 1, 0))
    CriticalValBoot <- quantile(boots.reps, probs = c(0.9, 
                                                      0.95, 0.99))
  }
  args <- list()
  args$nboot <- nboot
  args$boot.type <- boot.type
  ret <- list()
  ret$args <- args
  ret$stat <- teststat
  ret$values <- lm01
  ret$ths <- gamma2
  ret$ths_range <- gammas_range
  ret$maxTh <- gamma2[which(lm01 == ret$stat)]
  ret$PvalBoot <- PvalBoot
  ret$CriticalValBoot <- CriticalValBoot
  ret$allBoots <- boots.reps
  ret$beta <- ve$model.specific$coint[2, 1]
  class(ret) <- "TVECMHanSeo02Test"
  return(ret)
}

hansen2 = TVECM_rest(datos, lag=7, ngridTh = 100, trim=0.05, nboot=100,
fixed.beta =NULL)

plot(hansen2)

## ----fig-5.9,echo=F,fig.cap='Modelo de corrección del error por umbrales', out.width='4.5in', out.height='4.5in', fig.align='center', message=FALSE, fig.pos='!htpb'----

if (!require(tsDyn)) install.packages("tsDyn")

mono = TVECM(datos, lag=7, nthresh = 1, trim=0.05, ngridBeta = 200, ngridTh = 200, plot=TRUE, include=c("none"), common = "All", beta = list(int = c(-0.5,1.5)), beta0=rep(1,nrow(datos)))

#TVECM.sim(TVECMobject = mono, type = "boot")

## ------------------------------------------------------------------------
layout(matrix(c(1,1,1,1,1,
                2,2,2,2,2,
                3,3,0,4,4),3,5, byrow=TRUE))
plot(ts(residuals(mono_rest)[,1]), main = "Residuos de la ecuación mayorista")
plot(ts(residuals(mono_rest)[,1]), main = "Residuos de la ecuación supermercado")
Acf(residuals(mono_rest)[,1], main = "ACF mayorista")
Acf(residuals(mono_rest)[,2], main = "ACF supermercado")


## ----fig-5.11,echo=F,fig.cap='Histograma bivariado de los residuos del modelo', out.width='3.5in', out.height='3.5in', fig.align='center', message=FALSE, fig.pos='!htpb'----

res = residuals(mono_rest)

x = res[,1]
y = res[,2]

x_c = cut(x,20)
y_c = cut(y,20)

z = table(x_c,y_c)

hist3D(z=z, border="black", contour=TRUE)


## ----fig-5.12,echo=F,fig.cap='Mapa de calor de los residuos del modelo', out.width='3.5in', out.height='3.5in', fig.align='center', message=FALSE, fig.pos='!htpb'----
image2D(z=z, border="black")

## ----echo = FALSE--------------------------------------------------------
errores = data.frame(residuals(mono_rest))
DH.test(errores, names(errores))
vars:::.jb.multi(scale(residuals(mono_rest)), obs = 491, K=2, obj.name = deparse(substitute(mono_rest)))
vars:::.arch.multi(scale(residuals(mono_rest)), lags.multi = 8, K=2, obs = 491, obj.name = deparse(substitute(mono_rest)))

# Construiré una función adhoc basada en el código fuente del contraste de Ljung Box
# que se encuentra en el paquete vars
resids = residuals(mono)
K = mono$k
obs = mono$T
lags.pt = 20
ljung = function (K, obs, lags.pt, resids) 
{
    C0 <- crossprod(resids)/obs
    C0inv <- solve(C0)
    tracesum <- rep(NA, lags.pt)
    for (i in 1:lags.pt) {
        Ut.minus.i <- vars:::.matlag1(resids, lag = i)[-c(1:i), ]
        Ut <- resids[-c(1:i), ]
        Ci <- crossprod(Ut, Ut.minus.i)/obs
        tracesum[i] <- sum(diag(t(Ci) %*% C0inv %*% Ci %*% C0inv))
    }
    vec.adj <- obs - (1:lags.pt)
    Qh <- obs * sum(tracesum)
    Qh.star <- obs^2 * sum(tracesum/vec.adj)
    nstar <- K^2 * x$p
    STATISTIC <- Qh
    PARAMETER <- (K^2 * lags.pt - nstar + x$K)
    names(PARAMETER) <- "df"
    PVAL <- 1 - pchisq(STATISTIC, df = PARAMETER)
}

