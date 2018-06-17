rm(list=ls())

data(zeroyld)
est.res <- TVAR(zeroyld, lag=2, nthresh=2, thDelay=1, trim=0.1, mTh=1, plot=F)
ImpulseResp <- GIRF(est.res, n.hist=300, replic=100)
plot(ImpulseResp, response=2, ci=F)

#res <- est.res; hor=20; shk=-1; replic=100; shVar=2; i =1; n.hist=500; ci=c(0.05,0.95); quiet=F
GIRF <- function(res, hor=20, shk=1, shVar = 1, n.hist =500, replic=100, ci=c(0.025,0.975), quiet=F) {
  #res...estimation result from TVAR
  #hor...impulse response horizon
  #shk...shock size 
  #shVar...shocked Variable
  #n.hist...number of bootstraped histories in each regime
  #replic...bootstrap replications for each history
  #ci...confidence interval (two values needed!)
  #quiet...messages & progressbar?
  if (!quiet) cat("Shocked variable:", colnames(res$model)[shVar], "\nShock Size:", shk, "\n")
  
  #Regimes (NA is deleted later)
  regimes <- regime(res)
  
  #we need horizon + 1 residuals (+1 for current period) -> increase hor by 1
  #we take a particular history (i.e. the lagged values of a specific point) in a regime and predict 
  #its current value and n periods in the future
  #based on the model and th history-->  increase horizon by 1
  hor <- hor+1
  
  threshV <- which(res$model.specific$transCombin == 1) #which variable is used as threshold variable
  datS <- res$model[,1:res$k] #get dataset
  rownames(datS) <- 1:nrow(datS)
  
  #starting positions for simulations --> sample of observations w.r.t. regime are saved in hist.pos
  #in the end we start in period (hist.pos[,i]-1) in SimTVAR! 
  #hist.pos samples through observations according to their regime. But then we have to use the history
  #of the observations to genearte the IR (thus we start in (hist.pos[,i]-1) --> therefore we also
  #need hor+1 residuals)
  hist.pos <- matrix(NA, nrow=n.hist, ncol=max(regimes, na.rm=T))
  for (i in 1:max(regimes, na.rm=T)) {
    regime.rows <- suppressWarnings(as.numeric(rownames(datS[regimes==i,])))
    regime.rows <- regime.rows[!is.na(regime.rows)]
    
    hist.pos[, i] <- sample(regime.rows, size=n.hist, replace = T ) #sample through positions
  }

  #deleting NAs from regimes
  regimes <- regimes[!is.na(regime(res))]
  
  #===============
  #RESIDUALS
  #==============
  
  # Var-Covar of residuals
  Sigma <- (t(res$residuals) %*% res$residuals) / dim(res$residuals)[1] 
  
  #Cholesky
  P <- t( chol(Sigma) ) #lower triangular (chol(Sigma) is upper triangular)
  P.inv <- solve(P)
  str_err <- t( P.inv %*% t(res$residuals) ) #structural errors (transformed back in SimTVAR function for sim)
  
  #sample bootstrap residuals
  #number of bootstrap residuals needed (replications per history * n.history * horizon)
  boot_size <- replic * n.hist * hor 
  boot_sample <- array(dim=c(boot_size, res$k, max(regimes))) 
  for (i in 1:max(regimes)) {
    #position of bootsample to keep observations together
    boot_pos <- sample(x = 1:dim(str_err)[1], size = boot_size, replace = TRUE) 
    boot_sample[ , ,i] <- str_err[boot_pos, ] #samples
  }
  

  #===============
  #Simulation
  #==============
  
  #results for each history will be saved here
  avgDiff <- array(dim = c(hor, ncol(datS), n.hist), NA) 
  #final results -> girf and confidence intervals
  girf <- array(dim = c(hor, ncol(datS), max(regimes)), NA,  
                dimnames = list(1:hor, colnames(datS), names(res$coefficients) ))
  ci_lo <- array(dim = c(hor, ncol(datS), max(regimes)), NA,  
                dimnames = list(1:hor, colnames(datS), names(res$coefficients) ))
  ci_hi <- array(dim = c(hor, ncol(datS), max(regimes)), NA,  
                 dimnames = list(1:hor, colnames(datS), names(res$coefficients) ))
  
  if (!quiet) prog_b <- txtProgressBar(min=0, max= max(regimes)*n.hist, style=3)
  
  #simulate over all regimes
  for (i in 1:max(regimes)) {
    
    #simulate over all sampled histories within a regime
    for (j in 1:n.hist){
      startV <- hist.pos[j, i]
      #get the residuals for specific history (size= replic * horizon)
      resid_boot <- boot_sample[(replic*hor*(j-1)+1) : (replic*hor*j), , i] 
      
      #call simTVAR (second function)
      resultDiff <- lapply(1:replic, simTVAR, res, datS, hor, shk, threshV, 
                           shVar, startV, resid_boot, P)
      
      avgDiff[, , j] <- Reduce("+", resultDiff) / replic #saving means of all histories of part. regime
      
      if (!quiet) setTxtProgressBar(prog_b, n.hist*(i-1)+j)
    }
    
    #GIRF for regimes (final result) & Confidence interval
    ci_lo[, , i] <- apply(avgDiff, MARGIN = c(1,2), quantile, ci[1])
    ci_hi[, , i] <- apply(avgDiff, MARGIN = c(1,2), quantile, ci[2])
    girf[, , i] <- apply(avgDiff, MARGIN=c(1, 2), mean) 

  }
  
  if (!quiet) close(prog_b)
  
  #return Value
  return_val <- list(girf = girf, ci_lo = ci_lo, ci_hi = ci_hi, horizon = hor, 
                     shockedVar = shVar, shocksize = shk, estres=res)
  class(return_val) <- "girf"
  return(return_val)
}  


#k=1; results=res;dat=datS;horizon=hor;sh=shk;shvar=shVar;history=startV; r_boot <- resid_boot; Pm<-P
simTVAR <- function(k, results, dat, horizon, sh, threshV, shvar, history, r_boot, Pm) {
  #k...for apply function (index)
  #dat...data
  #horizon...impulse response horizon
  #sh...shock size (in standard deviations)
  #threshV...which variable is used as threshold Variable
  #shVar...shocked Variable
  #history...history of variable
  
  #Bootstrap errors
  boot_err <- r_boot[(horizon*(k-1)+1): (horizon*k), ]
  shock <- c(rep(0, shvar-1), sh, rep(0,nrow(results$coeffmat)-shvar))
  boot_err_delta <- rbind(boot_err[1,] + shock, boot_err[-1,])
  #boot_err_delta <- rbind(shock, boot_err[-1,])
  
  #Transform back -> Cholesky decomp
  boot_err <- t( Pm %*% t(boot_err) )
  boot_err_delta <- t( Pm %*% t(boot_err_delta) )
  
  #simulation without addtional shock --> innov = resid
  #HISTORY IS LAGGED BY 1 S.T. WE GET THE LAGGED VALUES!!!!!!!
  simul <- TVAR.sim(B = results$coeffmat, 
                    Thresh = results$model.specific$Thresh, 
                    nthres = results$model.specific$nthresh,
                    n = horizon, lag = results$lag, include = results$include,
                    thDelay = results$model.specific$thDelay, mTh = threshV,
                    starting = dat[(history - results$lag) : (history-1), ], innov = boot_err)
  
  #starting = dat[(history - results$lag + 1) : history, ], innov = boot_err)                  
  #starting = dat[(history - results$lag) : (history-1), ], innov = boot_err)
  
  #with resid_delta
  simul_delta <- TVAR.sim(B = results$coeffmat, 
                          Thresh = results$model.specific$Thresh, 
                          nthres = results$model.specific$nthresh,
                          n = horizon, lag = results$lag, include = results$include,
                          thDelay = results$model.specific$thDelay, mTh = threshV,
                          starting = dat[(history - results$lag) : (history-1), ],
                          innov = boot_err_delta)
  #starting = dat[(history - results$lag + 1) : history, ], innov = boot_err_delta)                        
  #starting = dat[(history - results$lag) : (history-1), ], innov = boot_err_delta)
  
  difference <- simul_delta - simul
  
  return(difference)  
}

plot.girf <- function(impR, response = 1, ci=T) {
  impulse <- impR$girf
  ci_l <- impR$ci_lo
  ci_h <- impR$ci_hi
  
  if (is.character(response)) suppressWarnings(response <- which(colnames(impulse)==response))
  
  for (i in response) {
    plot(impulse[ , i, 'Bdown'], type='l', ylim = c(min(impulse[, i, 1:dim(impulse)[3]]),
                                                    max(impulse[, i, 1:dim(impulse)[3]])),
         main = paste('Response of ', colnames(impulse)[i], ' to shock of ', 
                      colnames(impulse)[impR$shockedVar], sep=""), 
         ylab="response", xlab="horizon")
    lines(impulse[, i, 'Bup'], col='blue')
    abline(h=0, lty=4, col='red')
    
    if (ci) {
      lines(ci_l[, i, 'Bdown'], col='red', lty=2)
      lines(ci_h[, i, 'Bdown'], col='red', lty=2)
      lines(ci_l[, i, 'Bup'], col='red', lty=2)
      lines(ci_h[, i, 'Bup'], col='red', lty=2)
    }
    
    if (dim(impulse)[3] == 3) { #3 regimes?
      lines(impulse[ , i, 'Bmiddle'], type='l', col='green') #middle regime only with 3 regimes
      
      if (ci) {
        lines(ci_l[, i, 'Bmiddle'], col='red', lty=2)
        lines(ci_h[, i, 'Bmiddle'], col='red', lty=2)
      }
      
      legend("bottomright",cex = .8,ncol=1, pch=20,
             c("low","middle", "high"), 
             lty=c(1,1), 
             lwd=c(2.5,2.5),col=c("black","green","blue")) 
    } else {
      legend("bottomright",cex = .8,ncol=1, pch=20,
             c("low", "high"), 
             lty=c(1,1), 
             lwd=c(2.5,2.5),col=c("black", "blue")) 
    }
    
  }
 
}


  