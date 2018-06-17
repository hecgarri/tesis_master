##  generalized_impulse_response.R
##  
##  Héctor Garrido Henríquez
##  Analista Cuantitativo. Observatorio Laboral Ñuble
##  Docente. Facultad de Ciencias Empresariales
##  Universidad del Bío-Bío
##  Avenida Andrés Bello 720, Casilla 447, Chillán
##  Teléfono: +56-942353973
##  http://www.observatoriolaboralnuble.cl
##
##  Este programa estima una función de impulso respuesta generalizada
##  para el modelo TVECM

#k=1; results=res;dat=datS;horizon=hor;sh=shk;shvar=shVar;history=1;
simTVECM <- function(k=1, results=res, dat=datS, horizon = hor, sh=1, shvar=1, history=1) {
  #k...for apply function (index)
  #dat...data
  #horizon...impulse response horizon
  #sh...shock size (in standard deviations)
  #threshV...which variable is used as threshold Variable
  #shVar...shocked Variable
  #history...history of variable
  
  
  #sample bootstrap residuals
  resid <- matrix(NA, nrow=horizon, ncol=results$k)
  for (i in 1:results$k) resid[,i] <- sample(results$residuals[,i], size=horizon, replace=TRUE)
  #resid <- sample(results$residuals, size=horizon * nrow(results$coeffmat), replace=TRUE)
  #dim(resid) <- c(horizon,nrow(results$coeffmat))
  
  #bootstrap residuals for shocked series (same residuals with additional shock at the beginning)
  shock <- sqrt(var(results$residuals[, shvar])) * sh #shock at t
  shock <- c(rep(0, shvar-1), shock, rep(0,nrow(results$coeffmat)-shvar))
  resid_delta <- rbind(resid[1,] + shock, resid[-1,])
  
  #simulation without addtional shock --> innov = resid
  simul <- TVECM.sim(B = results$coeffmat, 
                     Thresh = results$model.specific$Thresh, 
                     nthres = results$model.specific$nthresh,
                     n = horizon, lag = results$lag, include = results$include,
                     beta = results$model.specific$beta,
                     innov = resid)
  
  #with resid_delta
  simul_delta <- TVECM.sim(B = results$coeffmat, 
                           Thresh = results$model.specific$Thresh, 
                           nthres = results$model.specific$nthresh,
                           n = horizon, lag = results$lag, include = results$include,
                           beta = results$model.specific$beta,
                           innov = resid_delta)
  diff <- simul_delta - simul
  return(diff)
}


require(parallel)

res = mono
datS = datos
hor = 62
sh = 1
shvar = 1
replic = 1000

avgDiff <- array(dim = c(hor, ncol(datS), (nrow(datS) - res$lag)), NA)
for (i in 1 : (nrow(datS) - res$lag)) {
  resultDiff <- mclapply(1:replic, simTVECM, res, datS, hor, sh, shvar,i,mc.cores = 4) #call simTVAR (second function)
  avgDiff[, , i] <- Reduce("+", resultDiff) / replic #mean of particular history
}

regimes <- regime(res)[(res$lag + 2) : length(regime(res))]

#GIRF for regimes (final result)
girf <- array(dim = c(hor, ncol(datS), max(regimes)), NA)
for (i in 1 : max(regimes)) {
  selectReg <- avgDiff[, , which(regimes == i)]
  girf[, , i] <- apply(selectReg, MARGIN=c(1, 2), sum,na.rm = TRUE) / dim(selectReg)[3]
}

### Para el caso de los precios mayoristas el shock es de aproximadamente 
### un aumento de 8% en el precio.

layout(matrix(c(1,2),nrow=1,ncol =2, byrow=TRUE))

colnames(girf) = c("% Mayoristas", "% Supermercados")
plot.ts(girf[(res$lag+2):hor,,1],
        main = "IRF ortogonal desde mayoristas, primer regimen",
        xy.labels =FALSE, 
        xlab = "Semanas", family = "Serif")
plot.ts(girf[(res$lag+2):hor,,2],
        main = "IRF ortogonal desde mayoristas, segundo regimen",
        xy.labels =FALSE, 
        xlab = "Semanas", family = "Serif")

