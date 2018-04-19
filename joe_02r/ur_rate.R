###########################################################################
##
##  UR_RATE.R
##  Specification tests for a threshold bi-variate VECM
##  on term structure data
##  
##  written by:
##  
##  Bruce E. Hansen
##  Department of Economics
##  Social Science Building
##  University of Wisconsin
##  Madison, WI 53706-1393
##  behansen@wisc.edu
##  http://www.ssc.wisc.edu/~bhansen/
##  
###########################################################################

#*************************************************************#
trim <- .05		# trimming percentage for threshold 
boot <- 5000	# number of bootstrap replications 
#*************************************************************#

source("tar.R")
dat <- read.table("zeroyld.dat")
datstore <- dat[1:nrow(dat),(7:62)]
rs <- rbind(as.matrix(seq(0,18,1)),21,24,30,as.matrix(seq(36,(36+7*12),12)))
rates_i <- matrix(c(1,1,1,3,3,3,12,12,24,2,3,6,6,12,120,24,120,120),9,2)

for (k in 1:2){ # Lags in VAR beyond EC #

for (ri in 1:nrow(rates_i)){

short <- rates_i[ri,1]
long <- rates_i[ri,2]
short_i <- which.max(rs==short)
long_i <- which.max(rs==long)
dat <- datstore[,cbind(long_i,short_i)]    

if (k==1&ri==1){
cat ("**********************************************************", "\n")
cat ("\n")
cat ("Number of Bootstrap Replications    ", boot, "\n")
cat ("\n")
cat ("\n")
}
cat ("**********************************************************", "\n")
cat ("\n")
cat ("Long Rate  (month):     ", long, "\n")
cat ("Short Rate (month):     ", short, "\n")
cat ("Number of VAR lags:     ", k, "\n")
cat ("\n")

# Cointegration Tests #
# Known CI vector #
y <- as.matrix(dat[,1] - dat[,2])
t <- nrow(y)-1-k
# ADF test #
dy <- as.matrix(y[(2+k):(t+k+1)]-y[(1+k):(t+k)])
x <- cbind(y[(1+k):(t+k)],matrix(1,t,1))  
for (j in 1:k) x <- cbind(x,(y[(2+k-j):(t+k+1-j)]-y[(1+k-j):(t+k-j)]))
rho <- qr.solve(x,dy)
e <- dy-x%*%rho
v <- solve(t(x)%*%x)*as.vector((t(e)%*%e)/(t-k-2))
adf <- rho[1]/sqrt(v[1,1])
cat ("ADF test                ",adf,"\n")
cat ("\n")

# Estimated CI vector #
y <- as.matrix(dat[,1])
x <- cbind(matrix(1,nrow(y),1),dat[,2])
beta <- qr.solve(x,y)
y <- y-x%*%beta
t <- nrow(y)-1-k
# Engle-Granger test #
dy <- as.matrix(y[(2+k):(t+k+1)]-y[(1+k):(t+k)])
x <- cbind(y[(1+k):(t+k)],matrix(1,t,1)) 
for (j in 1:k) x <- cbind(x,(y[(2+k-j):(t+k+1-j)]-y[(1+k-j):(t+k-j)]))
rho <- qr.solve(x,dy)
e <- dy-x%*%rho
v <- solve(t(x)%*%x)*as.vector((t(e)%*%e)/(t-k-2))
adf <- rho[1]/sqrt(v[1,1])
cat ("Estimated CI vector	", t(beta), "\n")
cat ("Engle-Granger test 	", adf, "\n")
cat ("\n")

# Nonlinear Tests #
y <- as.matrix(dat[,1] - dat[,2])
out <- tar(y,k+1,0,trim,1-trim,boot,0,0)
cat ("Test for TAR in EC - Known CI Vector ", out$pvalues[1],"\n")
y <- dat[,1]-cbind(matrix(1,nrow(y),1),dat[,2])%*%beta 
out <- tar(y,k+1,0,trim,1-trim,boot,0,0)
cat ("Test for TAR in EC - Estimated CI Vector ", out$pvalues[1],"\n")
cat ("\n")
cat ("\n")
}
}
