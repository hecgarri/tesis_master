##  resultados.R
##  
##  Héctor Garrido Henríquez
##  Analista Cuantitativo. Observatorio Laboral Ñuble
##  Docente. Facultad de Ciencias Empresariales
##  Universidad del Bío-Bío
##  Avenida Andrés Bello 720, Casilla 447, Chillán
##  Teléfono: +56-942353973
##  http://www.observatoriolaboralnuble.cl
##
##  Este programa es probablemente el más importante de la tesis y en él
##  se reproducen todos los cálculos necesarios para obtener las figuras  
##  y las tablas de la sección de resultados

setwd("/home/hector/GoogleDrivePersonal/Master/Tesis/GitHub/tesis_master")
source("weekly_palta.R")

if (!require(vars)) install.packages("vars"); require(vars)
if (!require(tsDyn)) install.packages("tsDyn"); require(vars)
if (!require(ggplot2)) installed.packages("ggplot2"); require(ggplot2)
if (!require(tidyverse)) install.packages("tidyverse"); require(tidyverse)
if (!require(forecast)) install.packages("forecast"); require(forecast)

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


datos = data.frame(y1,y2)
names(datos) = c("mayorista","supermercado")

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

p1 = ggplot(meltresultado, aes(x=rezago, y=`p-value`, colour=Modelo, group=Modelo)) + 
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


## Pruebo dos formulaciones equivalentes 
modelo_alternativo = VECM(datos, lag=7, r=1, LRinclude= c("const"), 
                          estim = c("ML"))

modelo_ = ca.jo(datos, type = c("trace"), ecdet = c("const"), K=8,
                spec = c("transitory"))

modelo1_ = ca.jo(datos, type = c("eigen"), ecdet = c("const"), K=8,
                 spec = c("transitory"))

modelo__=cajorls(modelo_,r=1)
summary(modelo__$rlm)

res_mayo = summary(modelo__$rlm)$`Response mayorista.d`$coefficients[,c(1,2)] %>% data.frame()
res_super = summary(modelo__$rlm)$`Response supermercado.d`$coefficients[,c(1,2)] %>% data.frame()

vecm = merge(res_mayo[,c(1,2)], res_super[,c(1,2)], by=0)
var_names = paste0("Delta",vecm$Row.names,"$_{t-1}$")
vecm = round(vecm[,-1],3) 

vecm = paste0(var_names," & ",vecm[,1],"(",vecm[,2],") & ",vecm[,3],"(",vecm[,4],") \\") %>%
  data.frame() 

## ------------------------------------------------------------------------
not_restrict = VECM(datos, lag=7, estim="ML", r=1, LRinclude = c("const"))

## Contraste de homogeneidad en precios

modelo_ = ca.jo(datos, type = c("trace"), ecdet = c("const"), K=8,
                spec = c("transitory"))
H0=matrix(c(1,-1,0,0,0,1),c(3,2))
homogeneity = blrtest(modelo_,H=H0,r=1)

## ------------------------------------------------------------------------
# Para obtener la constante restringida 
restrict = cajorls(homogeneity,r=1)



## ------------------------------------------------------------------------
homogeneity = blrtest(modelo_,H=H0,r=1)

weak_exogeneity = alrtest(modelo_,A=c(0,1),r=1)

## ----echo = F------------------------------------------------------------
var_version = vec2var(modelo_, r= 1)

impulso = irf(var_version, n.ahead = 52)
impulso2 = irf(var_version, n.ahead = 52, ortho = FALSE)

## ----graph5.6, echo=FALSE, fig.cap='Función ortogonal de impulso respuesta\\label{graph5.6}', fig.pos='!htpb'----
source("plot.varirf.R")
plot.varirf1(impulso)

#dev.print(pdf,"fig_results/fig3.pdf")

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

#dev.print(pdf,"fig_results/fig4.pdf")

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

#dev.print(pdf,"fig_results/fig5.pdf")

## ----include = FALSE, echo = FALSE---------------------------------------

if (!require(asbio)) install.packages("asbio")

errores = data.frame(residuals(var_version))
DH.test(errores, names(errores))
normality.test(var_version)
arch.test(var_version, lags.multi = 9)

window = 300
T = window
n = modelo_@P
k = modelo_@lag
sf = T/(T-n*k)
total = nrow(datos)
one_relation = sapply(0:(total-window), function(i) ca.jo(datos[i:(i+window),],
                            type=c("trace"), ecdet=c( "const"),
                            K=8, spec="transitory")@teststat[1]/modelo_@cval[1,2]*sf)

zero_relation = sapply(0:(total-window), function(i) ca.jo(datos[i:(i+window),],
                                        type=c("trace"), ecdet=c( "const"),
                                               K=8,
                                        spec="transitory")@teststat[2]/modelo_@cval[2,2]*sf)

ts.plot(cbind(zero_relation,one_relation), lty=1:2)
title(ylab="Estadístico de la traza")
abline(h=1)
price_may_palta[which(una_relacion>=1),c("day","month","year")]

ventana = lapply(1:350, function(i) i:(i+window))



## ------------------------------------------------------------------------
source('TVECM_HStest2.R')

if (!require(doMC)) install.packages("doMC"); require(doMC)
# Este es el contraste de Hansen y SEO con
#una constante en la relación de cointegración 
registerDoMC(4)
hansen1 = TVECM_HStest2(datos, lag = 7,
                       ngridTh = 100, trim = 0.05, intercept = FALSE,
                       LRinclude = "const", hpc = "foreach")


plot(hansen1)

#dev.print(pdf,"fig_results/fig6.pdf")

mono = TVECM(datos, lag=7, nthresh = 1, trim=0.05, ngridBeta = 200,
             ngridTh = 492, plot=TRUE, include=c("none"),
             common = "All", beta = list(int = c(-1.5,1.5)),
             beta0=rep(1,nrow(datos)), 
             methodMapply = FALSE)

mono_sum = summary(mono)

#dev.print(pdf,"fig_results/fig7.pdf")

coef_mono = round(rbind(t(mono_sum$coefficients$Bdown),t(mono_sum$coefficients$Bup)),3)

std_err_mono = round(rbind(t(mono_sum$StDev$Bdown),t(mono_sum$StDev$Bup)),3)

mono_tex = cbind(paste0(rownames(coef_mono)," & ",coef_mono[,1]," (",std_err_mono[,1],") ","&",
                 coef_mono[,2]," (", std_err_mono[,2],") \\")) %>% data.frame()
rownames(mono_tex) = NULL
#TVECM.sim(TVECMobject = mono, type = "boot")

## ------------------------------------------------------------------------
layout(matrix(c(1,1,1,1,1,
                2,2,2,2,2,
                3,3,0,4,4),3,5, byrow=TRUE))
plot(ts(residuals(mono2)[,1]), main = "Residuos de la ecuación mayorista")
plot(ts(residuals(mono2)[,1]), main = "Residuos de la ecuación supermercado")
Acf(residuals(mono2)[,1], main = "ACF mayorista")
Acf(residuals(mono2)[,2], main = "ACF supermercado")


## ----fig-5.11,echo=F,fig.cap='Histograma bivariado de los residuos del modelo', out.width='3.5in', out.height='3.5in', fig.align='center', message=FALSE, fig.pos='!htpb'----

res = residuals(mono2)

x = res[,1]
y = res[,2]

x_c = cut(x,20)
y_c = cut(y,20)

z = table(x_c,y_c)

hist3D(z=z, border="black", contour=TRUE)


## ----fig-5.12,echo=F,fig.cap='Mapa de calor de los residuos del modelo', out.width='3.5in', out.height='3.5in', fig.align='center', message=FALSE, fig.pos='!htpb'----
image2D(z=z, border="black")

## ----echo = FALSE--------------------------------------------------------
errores = data.frame(residuals(mono2))
DH.test(errores, names(errores))
vars:::.jb.multi(scale(residuals(mono2)), obs = 491, K=2, obj.name = deparse(substitute(mono2)))
vars:::.arch.multi(scale(residuals(mono2)), lags.multi = 8, K=2, obs = 491, obj.name = deparse(substitute(mono2)))

# Construiré una función adhoc basada en el código fuente del contraste de Ljung Box
# que se encuentra en el paquete vars
resids = residuals(mono2)
K = mono2$k
obs = mono2$T
lags.pt = 20
p = ncol(datos_rest)
ljung = function (K, obs, lags.pt, resids,p) 
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
  nstar <- K^2 * p
  STATISTIC <- Qh
  PARAMETER <- (K^2 * lags.pt - nstar + K)
  names(PARAMETER) <- "df"
  PVAL <- 1 - pchisq(STATISTIC, df = PARAMETER)
  PVAL
}

sapply(2:52, function(x) ljung(K,obs,x,resids,p))


