TVECM_HStest2 = function (data, lag = 1, ngridTh = 300, trim = 0.05, nboot = 100, 
          fixed.beta = NULL, intercept = FALSE,
          boot.type = c("FixedReg","ResBoot"), hpc = c("none", "foreach"),
          LRinclude = c("none","const","trend","both")) 
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
    ve <- VECM(data, lag = lag, LRinclude = "const", include = "none",
               estim = "ML")
  }
  else {
    ve <- VECM(data, lag = lag, LRinclude = "none", beta = fixed.beta, 
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
          VVinv <- ginv(VV)
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
          VVinv <- ginv(VV)
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
                          include = "none",
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
