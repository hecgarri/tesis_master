rm(list=ls())
setwd("/home/hector/GoogleDrivePersonal/Master/Tesis/GitHub/tesis_master")
source("weekly_palta.R")

y1 = precio_mayorista 
y2 = precio_supermercado

for(i in 1:1){
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
}

datos = cbind(y2,y1) %>% as.matrix
colnames(datos) = c("Supermercados", "Mayoristas")

data = datos

modelo_ = ca.jo(datos, type = c("trace"), ecdet = c("const"), K=8,
                spec = c("transitory"))
H0=matrix(c(1,-1,0,0,0,1),c(3,2))
homogeneity = blrtest(modelo_,H=H0,r=1)

y = data %>% as.matrix
p = 7
k = ncol(y)
betaLT = -homogeneity@V[-1,1]
X = cbind(y,1)
T = dim(y)[1]
t = T - p - 1
trim = 0.05
ngridG = 50
ndig = tsDyn:::getndp(y)
DeltaX = embed(diff(y), p + 1)[, -(1:k)]
Xminus1 = cbind(embed(y, p + 2)[, (k + 1):(k + k)],1) # variables endógenas rezagadas 
ECT =  X %*% c(1,-betaLT)  ## Aquí Estoy alterando 
ECT = round(ECT, ndig)          # Algunas partes del código
ECTminus1 = round(Xminus1 %*% c(1, -betaLT), ndig) 
Z = cbind(ECTminus1, DeltaX) # Esta matriz contiene el término de corrección 
# del error y los valores rezagados de las variables endógenas
DeltaY = diff(y)[(p + 1):(T - 1), ] # Diferencias de la variable endógenas 
Y = DeltaY
B = t(Y) %*% Z %*% solve(t(Z) %*% Z)
npar = ncol(B) # Número de parámetros 
allpar = ncol(B) * nrow(B) # Todos los parámetros
rownames(B) = paste("Equation", colnames(data))
LagNames = c(paste(rep(colnames(data), p), -rep(seq_len(p), each = k)))
colnames(B) = c("ECT",LagNames)
res = Y - Z %*% t(B) # Residuos del modelo lineal 
Sigma = matrix(1/t * crossprod(res), ncol = k, dimnames = list(colnames(data), colnames(data)))
VarCov = solve(crossprod(Z)) %x% Sigma # Matriz de varianzas y covarianzas
StDev = matrix(diag(VarCov)^0.5, nrow = k) # Desviaciones estándar
Tvalue = B/StDev # Estadístico T 
Pval = pt(abs(Tvalue), df = (t - ncol(Z)), lower.tail = FALSE) + 
  pt(-abs(Tvalue), df = (t - ncol(Z)), lower.tail = TRUE) # Valor p
colnames(Pval) = colnames(B)
allgammas = sort(unique(ECTminus1)) # Valores ordenados del ECT (para buscar parámetros gamma)
ng = length(allgammas) # Longitud del número de gammas
gammas = allgammas[round(seq(from = trim, to = 1 - trim, 
                              length.out = ngridG) * ng)] # Posibles valore de gamma a escoger
SSR = crossprod(c(Y - tcrossprod(Z, crossprod(Y,Z) %*% solve(crossprod(Z)))))


k = 7
n <- nrow(data)
y <- as.matrix(data[(2+k):n,]-data[(1+k):(n-1),])
t <- nrow(y)
xlag <- cbind(as.matrix(data[(1+k):(n-1),]),1)
x <- NULL
for (j in 1:k)  x <- cbind(x,data[(2+k-j):(n-j),]-data[(1+k-j):(n-1-j),])
x <- as.matrix(x)
bn = length(gammas)
xx <- solve(t(x)%*%x)
u <- y-x%*%xx%*%(t(x)%*%y)

store = NULL
for (bj in 1:bn){
  gam = gammas[bj]
  beta <- c(1,0.33)
  w <- xlag%*%c(1,(-beta))
  z <- cbind(w,x)
  d1 <- (w <= gam)
  n1 <- sum(d1)
  if (min(rbind(n1,(t-n1)))/t > trim){
    zj <- cbind((z*(d1%*%matrix(1,1,ncol(z)))),w)
    zzj <- as.matrix(zj-x%*%xx%*%(t(x)%*%zj))
    if (qr(zzj)$rank==ncol(zzj)) bz <- qr.solve(zzj,u)
    if (qr(zzj)$rank<ncol(zzj)) bz <- (qr(t(zzj)%*%zzj)$qr)%*%(t(zzj)%*%u)
    store[bj] <- (t/2)*log(det(t(u-zzj%*%bz)%*%(u-zzj%*%bz)/t))
  }
}

plot(store~gammas,type="l")
points(gammas[which.min(store)],store[which.min(store)],col="red",cex=2)




for (j in 1:k)  x <- cbind(x,x[(2+k-j):(n-j),]-x[(1+k-j):(n-1-j),])

gammahat = gammas[which.min(store)]
w <- xlag%*%c(1,(-beta))
z <- cbind(w,x)
d1 <- as.matrix((w <= gammahat))
d2 <- 1-d1
y1 <- as.matrix(y[d1%*%matrix(1,1,ncol(y))>0])
y1 <- matrix(y1,nrow(y1)/ncol(y),ncol(y))
z1 <- as.matrix(z[d1%*%matrix(1,1,ncol(z))>0])
z1 <- matrix(z1,nrow(z1)/ncol(z),ncol(z))
y2 <- as.matrix(y[d2%*%matrix(1,1,ncol(y))>0])
y2 <- matrix(y2,nrow(y2)/ncol(y),ncol(y))
z2 <- as.matrix(z[d2%*%matrix(1,1,ncol(z))>0])
z2 <- matrix(z2,nrow(z2)/ncol(z),ncol(z))
zz1 <- solve(t(z1)%*%z1)
zz2 <- solve(t(z2)%*%z2)
beta1 <- zz1%*%(t(z1)%*%y1) 
beta2 <- zz2%*%(t(z2)%*%y2)
e1 <- y1 - z1%*%beta1
e2 <- y2 - z2%*%beta2
