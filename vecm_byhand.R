cat ("**********************************************************", "\n")
cat ("\n")
cat ("Number of Bootstrap Replications    ", boot, "\n")
cat ("Number of Gridpoints for threshold  ", gn, "\n")
if (coint==1){
  cat ("Estimated Cointegrating Vector", "\n")
  cat ("Number of Gridpoints for ci vector ", bn, "\n")
}else{
  cat ("Cointegrating Vector fixed at      ", cvalue, "\n")
}
cat ("\n")
cat ("\n")
cat ("Long Rate  (month):     ", long, "\n")
cat ("Short Rate (month):     ", short, "\n")
cat ("Number of VAR lags:     ", k, "\n")
cat ("\n")
}

# Organize Data #
n <- nrow(dat)
y <- as.matrix(dat[(2+k):n,]-dat[(1+k):(n-1),]) # Equivalente al operador de diferencias.
t <- nrow(y)
xlag <- as.matrix(dat[(1+k):(n-1),]) # variable rezagada
x <- matrix(1,t,1) # intercepto
for (j in 1:k)  x <- cbind(x,(dat[(2+k-j):(n-j),]-dat[(1+k-j):(n-1-j),])) # matriz de datos
x <- as.matrix(x)

# Compute Linear Model #
xx <- solve(t(x)%*%x)
xxx <- x%*%xx
u <- y-x%*%xx%*%(t(x)%*%y) # este paso involucra al estimador OLS, lleva al modelo concentrado
if (coint==1){
  v <- xlag-x%*%xx%*%(t(x)%*%xlag)
  uu <- t(u)%*%u
  m <- solve(t(v)%*%v)%*%(t(v)%*%u)%*%solve(uu)%*%(t(u)%*%v)
  ev <- eigen(m)
  va <- ev$val
  ve <- ev$vec
  h <- ve[,which.max(va)]
  b0 <- -h[2]/h[1] # aquí es donde se procede a normalizar el vector
}else{
  b0 <- cvalue
}
w0 <- as.matrix(xlag%*%rbind(1,-b0)) # Esta es la relación de cointegración o ECM
z0 <- cbind(w0,x) # Este es la matriz con los términos apilados
kk <- ncol(z0)
zz0 <- solve(t(z0)%*%z0)
zzz0 <- z0%*%zz0
beta0 <- t(zzz0)%*%y # esta matriz permite mirar los términos de corto plazo. 
e <- y - z0%*%beta0 
sige <- t(e)%*%e/t # Matriz de covarianza estimada
nlike <- (t/2)*log(det(sige)) # Función de verosimilitud 
bic <- nlike+log10(t)*4*(1+k) # criterio bic 
aic <- nlike+2*4*(1+k) # criterio aic
b_like <- function(b){
  z <- cbind((xlag%*%rbind(1,-b)),x) 
  yz <- y - z%*%qr.solve(z,y)
  sigma <- (t(yz)%*%yz)/t  
  like <- (t/2)*log(det(sigma))
  like
}
nlike1 <- b_like(b0+.001)
nlike2 <- b_like(b0-.001)
hp <- (nlike1+nlike2-2*nlike)/(.001^2)
seb <- 1/sqrt(hp)
k_product <- function(ma,mb){
  mat <- matrix(0,nrow(ma)*nrow(mb),ncol(ma)*ncol(mb))
  for (i in 1:nrow(ma)){
    for (j in 1:ncol(ma)){
      mat[(1:nrow(mb))+(i-1)*nrow(mb),(1:ncol(mb))+(j-1)*ncol(mb)] <- mb*ma[i,j]
    }
  }
  mat
}