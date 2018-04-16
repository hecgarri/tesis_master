
res = mono 
hor = 20
shk = 1
replic = 100

GIRF <- function(res, hor=20, shk=1, shVar = 1, replic=100) {
  #res...estimation result from TVECM
  #hor...impulse response horizon
  #shk...shock size (in standard deviations)
  #shVar...shocked Variable
  #replic...no. bootstrap replications
  
  datS <- res$model[,1:res$k] #get dataset
  
  avgDiff <- array(dim = c(hor, ncol(datS), (nrow(datS) - res$lag)), NA) #results for each history will be saved here
  #simulate over all histories (regime dependence is dealt with in the next step)
  for (i in 1 : (nrow(datS) - res$lag)) {
    resultDiff <- lapply(1:replic, simTVAR, res, datS, hor, shk, threshV, shVar, i) #call simTVAR (second function)
    avgDiff[, , i] <- Reduce("+", resultDiff) / replic #mean of particular history
  }
  
  
  #Regimes
  regimes <- regime(res)[(res$lag + 1) : length(regime(res))]
  
  #GIRF for regimes (final result)
  girf <- array(dim = c(hor, ncol(datS), max(regimes[-1])), NA)
  for (i in 1 : max(regimes[-1])) {
    selectReg <- avgDiff[, , which(regimes == i)]
    girf[, , i] <- apply(selectReg, MARGIN=c(1, 2), sum) / dim(selectReg)[3]
  }
  
  return(girf)
}  

k = 2
n = 200

lala = TVECM.sim(B=res$coeffmat,nthresh = 1, 
                 Thresh =res$model.specific$Thresh,
          beta = res$model.specific$beta,n=200,lag=8,
          type = "simul", include = "none",
          innov = rmnorm(n, vcov = diag(1,k)))

ts.plot(lala)

##########################
# Example
##########################

if(FALSE){
  
  library(tsDyn)
  data(zeroyld)
  
  #par(mar = rep(2, 4))
  resT <- TVAR(zeroyld, lag=2, nthresh=2, thDelay=1, trim=0.1, mTh=1, plot=F)
  #test <- sample(c(1,0), 482, replace=T)
  #res <- TVAR(zeroyld, lag=2, nthresh=1, thDelay=1, thVar = test, Thresh=0)
  
  resGIRF <- GIRF(resT)
  dim(resGIRF)
  plot(cumsum(resGIRF[,2,3]), type='l')
  plot(resGIRF[,2,3], type='l')
}