function (data, lag = 1, nthresh = 1, trim = 0.05, ngridBeta = 50, 
          ngridTh = 50, plot = TRUE, th1 = list(exact = NULL, int = c("from", 
                                                                      "to"), around = "val"), th2 = list(exact = NULL, int = c("from", 
                                                                                                                               "to"), around = "val"), beta = list(exact = NULL, int = c("from", 
                                                                                                                                                                                         "to"), around = c("val", "by")), restr = c("none", "equal", 
                                                                                                                                                                                                                                    "signOp"), common = c("All", "only_ECT"), include = c("const", 
                                                                                                                                                                                                                                                                                          "trend", "none", "both"), dummyToBothRegimes = TRUE, 
          beta0 = 0, methodMapply = FALSE, trace = TRUE) 
{
  include <- match.arg(include)
  model <- match.arg(common)
  restr <- match.arg(restr)
  if (restr == "equal") 
    stop("Sorry, restriction 'equal' not yet fully implemented")
  bn <- ngridBeta
  ngridG <- ngridTh
  gamma1 <- th1
  gamma2 <- th2
  if (!missing(gamma1) && !is.list(gamma1)) 
    gamma1 <- list(exact = gamma1)
  if (!missing(gamma2) && !is.list(gamma2)) 
    gamma2 <- list(exact = gamma2)
  y <- as.matrix(data) # Aquí quedan guardados los datos
  T <- nrow(y) # El tamaño de la muestra completa
  p <- lag # El nnúmero de rezagos a utilizar 
  t <- T - p - 1 # El tamaño de la muestra en función de los rezagos 
  k <- ncol(y) # Numero de variables endógenas 
  if (k > 2 & is.null(beta$exact)) 
    stop("Sorry, the search is only possible with 2 variables. If more, please provide pre-specified beta values")
  if (is.null(colnames(data))) 
    colnames(data) <- paste("Var", c(1:k), sep = "")
  ndig <- getndp(y)
  ysmall <- y[(p + 1):T, ]
  DeltaY <- diff(y)[(p + 1):(T - 1), ]
  Xminus1 <- embed(y, p + 2)[, (k + 1):(k + k)]
  #####################################################
  # Estos son los valores rezagados de las variables del modelo
  ###################################################
  DeltaX <- embed(diff(y), p + 1)[, -(1:k)] 
  #####################################################
  # La matriz de los valores rezagados de las variables endógenas
  ####################################################
  if (include == "const") 
    DeltaX <- cbind(rep(1, t), DeltaX)
  else if (include == "trend") 
    DeltaX <- cbind(seq_len(t), DeltaX)
  else if (include == "both") 
    DeltaX <- cbind(rep(1, t), seq_len(t), DeltaX)
  #####################################################
  # Los argumentos "const", "trend" y "both" utilizan los términos
  # deterministas en la matriz de diseño. No en la relación de cointegración
  #####################################################
  beta0 <- as.matrix(beta0)
  if (is.null(beta$exact)) {
    if (beta0[1] != 0) {
      if (nrow(beta0) != nrow(y)) 
        stop("Length of beta0 should be ", nrow(y), "\n")
      coint <- lm(y[, 1] ~ y[, 2] + beta0 - 1)
      beta0 <- (beta0 %*% coint$coef[-1])[-c(1:p, T), ]
    }
    else {
      coint <- lm(y[, 1] ~ y[, 2] - 1)
      beta0 <- rep(0, t)
    }
    betaLT <- coint$coef[1]
    betaLT_std <- sqrt(diag(summary(coint)$sigma * summary(coint)$cov))[1]
  }
  else {
    betaLT <- beta$exact
    if (length(betaLT) != k - 1) 
      warning("beta$exact should be of same size as cols of y -1\n")
    if (beta0[1] != 0) {
      stop("Sorry, use of beta0 and beta$exact currently not supported simultaneously\n")
    }
    else {
      beta0 <- rep(0, t)
    }
  }
  ECT <- y %*% c(1, -betaLT) # Término de corrección del error. 
  ECT <- round(ECT, ndig)
  ECTminus1 <- round(Xminus1 %*% c(1, -betaLT), ndig)
  Z <- cbind(ECTminus1 - beta0, DeltaX)
  Y <- DeltaY
  B <- t(Y) %*% Z %*% solve(t(Z) %*% Z) # Esta es la estimación de mínimos cuadrados ordinarios 
  # del modelo de corrección del error. 
  npar <- ncol(B)
  allpar <- ncol(B) * nrow(B)
  rownames(B) <- paste("Equation", colnames(data))
  LagNames <- c(paste(rep(colnames(data), p), -rep(seq_len(p), 
                                                   each = k)))
  colnames(B) <- switch(include, const = c("ECT", "Intercept", 
                                           LagNames), 
                        trend = c("ECT", "Trend", LagNames), both = c("ECT", 
                    "Intercept", "Trend", LagNames), none = c("ECT", LagNames))
  res <- Y - Z %*% t(B)
  Sigma <- matrix(1/t * crossprod(res), ncol = k, dimnames = list(colnames(data), 
                                                                  colnames(data)))
  VarCov <- solve(crossprod(Z)) %x% Sigma
  StDev <- matrix(diag(VarCov)^0.5, nrow = k)
  Tvalue <- B/StDev
  Pval <- pt(abs(Tvalue), df = (t - ncol(Z)), lower.tail = FALSE) + 
    pt(-abs(Tvalue), df = (t - ncol(Z)), lower.tail = TRUE)
  colnames(Pval) <- colnames(B)
  allgammas <- sort(unique(ECTminus1 - beta0)) # Elimina duplicados y establece los umbrales como
  # la diferencia entre el término de corrección del error y el parámetros beta0
  ng <- length(allgammas)
  gammas <- allgammas[round(seq(from = trim, to = 1 - trim, 
                                length.out = ngridG) * ng)]
  if (is.null(gamma1$exact) == FALSE) {
    if (any(allgammas == gamma1$exact) == FALSE) 
      warning("The value you gave for gamma does not correspond to an existing value. This causes problems currently")
    gammas <- gamma1$exact
    ngridG <- 1
  }
  if (is.numeric(gamma1$int)) {
    intDown <- which.min(abs(allgammas - gamma1$int[1]))
    intUp <- which.min(abs(allgammas - gamma1$int[2]))
    gammas <- allgammas[seq(from = intDown, to = intUp, length.out = min(ngridG, 
                                                                         intUp - intDown))]
  }
  if (is.numeric(gamma1$around)) 
    gammas <- aroundGrid(gamma$around, allvalues = allgammas, 
                         ngridG, trim, trace = trace)
  gammas <- round(gammas, ndig)
  if (!is.null(beta$exact)) {
    betas <- matrix(beta$exact, nrow = 1)
    bn <- 1
  }
  else if (is.numeric(beta$int)) {
    betas <- matrix(seq(from = beta$int[1], to = beta$int[2], 
                        length.out = bn), ncol = 1)
  }
  else if (is.numeric(beta$around)) {
    by <- beta$around[2]
    betas <- matrix(seq(from = beta$around[1] - bn * by/2, 
                        to = beta$around[1] + bn * by/2, by = by), ncol = 1)
  }
  else {
    betas <- matrix(seq(from = betaLT - 2 * betaLT_std, to = betaLT + 
                          2 * betaLT_std, length.out = bn), ncol = 1)
  }
  #############################################
  # La función OneSearch busca la menor función de 
  # verosimilitud dados los valores de gamma y beta
  #############################################
  oneSearch <- function(betas, gammas) {
    
    if (dummyToBothRegimes == TRUE) {
      "%a%" <- function(matrix, dummy) matrix * dummy
    }
    else {
      "%a%" <- function(matrix, dummy) matrix
    }
    oneThresh <- function(betai, gam, Y, Xminus1, DeltaX) {
      ECTi <- Xminus1 %*% c(1, -betai) - beta0
      zi <- cbind(ECTi, DeltaX)
      d1 <- ifelse(ECTi <= gam, 1, 0)
      n1 <- mean(d1)
      if (is.na(n1) == TRUE) 
        n1 <- 0
      if (min(n1, 1 - n1) > trim) {
        zigamma <- c(d1) * zi
        zi <- zi %a% c(1 - d1)
        Z <- cbind(zigamma, zi)
        LS <- try(crossprod(c(Y - tcrossprod(Z, crossprod(Y, 
                                                          Z) %*% solve(crossprod(Z))))), silent = TRUE)
        if (inherits(LS, "try-error")) {
          warning("Error when solving for value: gamma=", 
                  gam, "and beta=", betai)
          LS <- NA
        }
      }
      else LS <- NA
      return(LS)
    }
    one_partial_Thresh <- function(betai, gam, Y, Xminus1, 
                                   DeltaX) {
      ECTi <- Xminus1 %*% c(1, -betai) - beta0
      d1 <- ifelse(ECTi <= gam, 1, 0)
      n1 <- mean(d1)
      if (min(n1, 1 - n1) > trim) {
        Z <- cbind(ECTi * d1, ECTi * (1 - d1), DeltaX)
        LS <- try(crossprod(c(Y - tcrossprod(Z, crossprod(Y, 
                                                          Z) %*% solve(crossprod(Z))))), silent = TRUE)
        if (inherits(LS, "try-error")) {
          warning("Error when solving for value: gamma=", 
                  gam, "and beta=", betai)
          LS <- NA
        }
      }
      else LS <- NA
      return(LS)
    }
    func_onethresh <- switch(model, All = oneThresh, only_ECT = one_partial_Thresh)
    if (methodMapply == FALSE) {
      store <- matrix(NA, nrow = length(gammas), ncol = nrow(betas), 
                      dimnames = list(round(gammas, 3), betas[, 1]))
      for (i in seq_len(length(gammas))) {
        gam <- gammas[i]
        for (j in seq_len(nrow(betas))) {
          betai <- betas[j, ]
          store[i, j] <- func_onethresh(betai = betai, 
                                        gam = gam, DeltaX = DeltaX, Xminus1 = Xminus1, 
                                        Y = Y)
        }
      }
      na <- sum(ifelse(is.na(store), 1, 0))
      if (na > 0) {
        if (trace) {
          cat(na, " (", percent(na/(nrow(store) * ncol(store)), 
                                3, by100 = TRUE), ") points of the grid lead to regimes with percentage of observations < trim and were not computed\n", 
              sep = "")
        }
      }
      pos <- which(store == min(store, na.rm = TRUE), arr.ind = TRUE)
      if (nrow(pos) > 1) {
        if (trace) {
          cat("There were ", nrow(pos), " thresholds/cointegrating combinations (", 
              paste(gammas[pos[, 1]], "/", betas[pos[, 
                                                     2], ], ", "), ") \nwhich minimize the SSR in the first search, the first one ", 
              round(gammas[pos[1, 1]], ndig), " ", round(betas[pos[1, 
                                                                   2], ], ndig), " was taken\n")
        }
        pos <- pos[1, ]
      }
      bestGamma1 <- gammas[pos[1]]
      beta_grid <- betas[pos[2], ]
    }
    if (methodMapply == TRUE) {
      if (ncol(betas) > 1) {
        stop("Method mapply does not work when there are more than 2 variables")
      }
      grid <- expand.grid(betas, gammas)
      ########################################
      # esta función genera un data.frame con todas las combinaciones posibles 
      # de beta y gamma
      #######################################
      oneThreshTemp <- function(betai, gam) func_onethresh(betai = betai, 
                                                           gam = gam, DeltaX = DeltaX, Xminus1 = Xminus1, 
                                                           Y = Y)
      storemap <- mapply(oneThreshTemp, betai = grid[, 
                                                     1], gam = grid[, 2])
      bests <- which(storemap == min(storemap, na.rm = TRUE))
      if (length(bests) > 1) {
        if (trace) {
          cat("There were ", length(bests), " thresholds values which minimize the SSR in the first search, the first one was taken\n")
        }
        bests <- bests[1]
      }
      beta_grid <- grid[bests, 1]
      bestGamma1 <- grid[bests, 2]
    }
    if (is.null(gamma1$exact) == FALSE & is.null(beta$exact) == 
        FALSE) {
      plot <- FALSE
    }
    if (plot) {
      if (!is.null(beta$exact) & is.null(gamma1$exact)) {
        plot(gammas, store, type = "l", xlab = "Threshold parameter gamma", 
             ylab = "Residual Sum of Squares", main = "Grid Search")
        points(x = bestGamma1, y = min(store, na.rm = TRUE), 
               col = 2, cex = 2)
      }
      if (is.null(beta$exact) & !is.null(gamma1$exact)) {
        plot(betas, store, type = "l", xlab = "Cointegrating parameter beta", 
             ylab = "Residual Sum of Squares", main = "Grid Search")
        points(x = beta_grid, y = min(store, na.rm = TRUE), 
               col = 2, cex = 2)
      }
      if (is.null(beta$exact) & is.null(gamma1$exact)) {
        options(warn = -1)
        betaRSS <- apply(store, 2, FUN = min, na.rm = TRUE)
        gammaRSS <- apply(store, 1, FUN = min, na.rm = TRUE)
        options(warn = 0)
        gammaRSS[is.infinite(gammaRSS)] <- NA
        betaRSS[is.infinite(betaRSS)] <- NA
        layout(c(1, 2))
        plot(gammas, gammaRSS, type = "l", xlab = "Threshold parameter gamma", 
             ylab = "Residual Sum of Squares", main = "Grid Search")
        points(x = bestGamma1, y = min(store, na.rm = TRUE), 
               col = 2, cex = 2)
        plot(betas, betaRSS, type = "l", xlab = "Cointegrating parameter beta", 
             ylab = "Residual Sum of Squares")
        abline(v = betaLT, lty = 3)
        points(x = beta_grid, y = min(store, na.rm = TRUE), 
               col = 2, cex = 2)
        legend("topright", "OLS estimate from linear VECM", 
               lty = 3, bg = "white")
      }
    }
    list(beta = beta_grid, gamma = bestGamma1)
  }
  if (nthresh == 1) {
    results <- oneSearch(betas, gammas)
    bestBeta <- results$beta
    bestThresh <- results$gamma
  }
  if (nthresh == 2) {
    two_Thresh <- function(betai, gam1, gam2) {
      ECTi <- Xminus1 %*% c(1, -betai) - beta0
      zi <- cbind(ECTi, DeltaX)
      d1 <- ifelse(ECTi <= gam1, 1, 0)
      n1 <- mean(d1)
      d2 <- ifelse(ECTi > gam2, 1, 0)
      n2 <- mean(d2)
      if (is.na(n1)) 
        n1 <- n2 <- 0
      if (min(n1, n2, 1 - n1 - n2) > trim) {
        ziUnder <- c(d1) * zi
        ziOver <- c(d2) * zi
        ziMiddle <- c(1 - d1 - d2) * zi
        Z <- cbind(ziUnder, ziMiddle, ziOver)
        LS <- try(crossprod(c(Y - tcrossprod(Z, crossprod(Y, 
                                                          Z) %*% solve(crossprod(Z))))), silent = TRUE)
        if (inherits(LS, "try-error")) {
          warning("Error when solving for value: gammas=", 
                  gam1, gam2, "and beta=", betai)
          LS <- NA
        }
      }
      else LS <- NA
      return(LS)
    }
    two_partial_Thresh <- function(betai, gam1, gam2) {
      ECTi <- Xminus1 %*% c(1, -betai) - beta0
      d1 <- ifelse(ECTi <= gam1, 1, 0)
      n1 <- mean(d1)
      d2 <- ifelse(ECTi > gam2, 1, 0)
      n2 <- mean(d2)
      if (is.na(n1)) {
        n1 <- 0
        n2 <- 0
      }
      if (min(n1, n2, 1 - n1 - n2) > trim) {
        ectUnder <- c(d1) * ECTi
        ectOver <- c(d2) * ECTi
        Z <- cbind(ectUnder, ectOver, DeltaX)
        result <- try(crossprod(c(Y - tcrossprod(Z, crossprod(Y, 
                                                              Z) %*% solve(crossprod(Z))))), silent = TRUE)
        if (inherits(result, "try-error")) {
          warning("Error when solving for value: gammas=", 
                  gam1, gam2, "and beta=", betai)
          result <- NA
        }
      }
      else result <- NA
      return(result)
    }
    bestone <- oneSearch(betas, gammas)
    bestThresh <- bestone$gamma
    bestBeta <- bestone$beta
    func <- switch(model, All = two_Thresh, only_ECT = two_partial_Thresh)
    if (trace) {
      cat("Best threshold from first search", bestThresh, 
          "\n")
      cat("Best cointegrating value", bestBeta, "\n")
    }
    if (!is.null(gamma2$exact)) 
      secondBestThresh <- gamma2$exact
    if (is.null(gamma2$exact) & !is.numeric(gamma2$around)) {
      wh.thresh <- which.min(abs(allgammas - bestThresh))
      ninter <- round(trim * nrow(Xminus1))
      if (restr == "none") {
        if (wh.thresh > 2 * ninter) {
          gammaMinus <- allgammas[seq(from = ninter, 
                                      to = wh.thresh - ninter)]
          if (k > 2) {
            storeMinus <- mapply(func, gam1 = gammaMinus, 
                                 gam2 = bestThresh, MoreArgs = list(betai = bestBeta))
          }
          else {
            storeMinus <- mapply(func, betai = bestBeta, 
                                 gam1 = gammaMinus, gam2 = bestThresh)
          }
        }
        else storeMinus <- NA
        if (length(wh.thresh < length(allgammas) - 2 * 
                   ninter)) {
          gammaPlus <- allgammas[seq(from = wh.thresh + 
                                       ninter, to = length(allgammas) - ninter)]
          storePlus <- mapply(func, gam2 = gammaPlus, 
                              MoreArgs = list(betai = bestBeta, gam1 = bestThresh))
        }
        else storePlus <- NA
      }
      else if (restr == "signOp") {
        zero <- which.min(abs(allgammas))
        if (sign(bestThresh) > 0) {
          gammaMinus <- allgammas[seq(from = ninter, 
                                      to = min(wh.thresh - ninter, zero))]
          if (k > 2) {
            storeMinus <- mapply(func, gam1 = gammaMinus, 
                                 gam2 = bestThresh, MoreArgs = list(betai = bestBeta))
          }
          else {
            storeMinus <- mapply(func, betai = bestBeta, 
                                 gam1 = gammaMinus, gam2 = bestThresh)
          }
          storePlus <- NA
        }
        else {
          gammaPlus <- allgammas[seq(from = max(wh.thresh + 
                                                  ninter, zero), to = length(allgammas) - ninter)]
          if (k > 2) {
            storePlus <- mapply(func, gam1 = bestThresh, 
                                gam2 = gammaPlus, MoreArgs = list(betai = bestBeta))
          }
          else {
            storePlus <- mapply(func, betai = bestBeta, 
                                gam1 = bestThresh, gam2 = gammaPlus)
          }
          storeMinus <- NA
        }
      }
      store2 <- c(storeMinus, storePlus)
      positionSecond <- which(store2 == min(store2, na.rm = TRUE))
      if (length(positionSecond) > 1) {
        if (trace) 
          cat("There were ", length(positionSecond), 
              " thresholds values which minimize the SSR in the conditional step, the first one was taken\n")
      }
      positionSecond <- positionSecond[1]
      if (positionSecond <= length(storeMinus)) {
        secondBestThresh <- gammaMinus[positionSecond]
      }
      else {
        secondBestThresh <- gammaPlus[positionSecond - 
                                        length(storeMinus)]
      }
      if (trace) 
        cat("Second best (conditionnal on the first one)", 
            c(bestThresh, secondBestThresh), "\t SSR", 
            min(store2, na.rm = TRUE), "\n")
    }
    bestThresh <- bestThresh
    secondBestThresh <- secondBestThresh
    if (is.numeric(gamma2$around)) 
      secondBestThresh <- gamma2$around
    smallThresh <- min(bestThresh, secondBestThresh)
    gammasDown <- aroundGrid(around = smallThresh, allgammas, 
                             ngrid = 30, trim = trim, trace = trace)
    bigThresh <- max(bestThresh, secondBestThresh)
    gammasUp <- aroundGrid(around = bigThresh, allgammas, 
                           ngrid = 30, trim = trim, trace = trace)
    if (!is.null(gamma2$exact)) {
      if (gamma2$exact < bestThresh) 
        gammasDown <- gamma2$exact
      if (gamma2$exact > bestThresh) 
        gammasUp <- gamma2$exact
    }
    if (!is.null(gamma1$exact)) {
      if (gamma1$exact < secondBestThresh) 
        gammasDown <- gamma1$exact
      if (gamma1$exact > secondBestThresh) 
        gammasUp <- gamma1$exact
    }
    storeIter <- matrix(NA, ncol = length(gammasUp), nrow = length(gammasDown))
    for (i in seq_along(gammasDown)) {
      gam1 <- gammasDown[i]
      for (j in seq_along(gammasUp)) {
        gam2 <- gammasUp[j]
        storeIter[i, j] <- func(gam1 = gam1, gam2 = gam2, 
                                beta = bestBeta)
      }
    }
    positionIter <- which(storeIter == min(storeIter, na.rm = TRUE), 
                          arr.ind = TRUE)
    if (nrow(positionIter) > 1) {
      if (trace) {
        cat("There were ", length(positionIter), " thresholds values which minimize the SSR in the iterative step, the first one was taken\n")
        positionIter <- positionIter[1, ]
      }
    }
    rIter <- positionIter[1]
    cIter <- positionIter[2]
    bestThresh1Iter <- gammasDown[rIter]
    bestThresh2Iter <- gammasUp[cIter]
    bestThresh <- c(bestThresh1Iter, bestThresh2Iter)
    if (trace) 
      cat("Second step best thresholds", bestThresh, "\t\t\t SSR", 
          min(storeIter, na.rm = TRUE), "\n")
  }
  if (nthresh == 1) {
    ECT_best <- Xminus1 %*% c(1, -bestBeta) - beta0
    Z_temp <- cbind(ECT_best, DeltaX)
    d1 <- ifelse(ECT_best <= bestThresh, 1, 0)
    ndown <- mean(d1)
    nup <- 1 - ndown
    if (model == "All") {
      Zunder <- c(d1) * Z_temp
      if (dummyToBothRegimes == TRUE) {
        Zover <- c(1 - d1) * Z_temp
      }
      else Zover <- Z_temp
      Zbest <- cbind(Zunder, Zover)
    }
    else {
      Zbest <- cbind(d1 * ECT_best, (1 - d1) * ECT_best, 
                     DeltaX)
    }
  }
  if (nthresh == 2) {
    ECT_best <- Xminus1 %*% c(1, -bestBeta) - beta0
    d1 <- ifelse(ECT_best <= bestThresh1Iter, 1, 0)
    ndown <- mean(d1)
    d2 <- ifelse(ECT_best > bestThresh2Iter, 1, 0)
    nup <- mean(d2)
    if (model == "All") {
      Z_temp <- cbind(ECT_best, DeltaX)
      Zunder <- c(d1) * Z_temp
      Zover <- c(d2) * Z_temp
      Zmiddle <- (1 - c(d1) - c(d2)) * Z_temp
      Zbest <- cbind(Zunder, Zmiddle, Zover)
    }
    else if (model == "only_ECT") {
      Zunder <- c(d1) * ECT_best
      Zover <- c(d2) * ECT_best
      Zbest <- cbind(Zunder, Zover, DeltaX)
    }
  }
  reg <- if (nthresh == 1) 
    d1 + 2 * (1 - d1)
  else d1 + 2 * (1 - d1 - d2) + 3 * d2
  regime <- c(rep(NA, T - t), reg)
  Bbest <- t(Y) %*% Zbest %*% solve(t(Zbest) %*% Zbest)
  allpar <- ncol(Bbest) * nrow(Bbest)
  fitted <- Zbest %*% t(Bbest)
  resbest <- Y - fitted
  rownames(Bbest) <- paste("Equation", colnames(data))
  DeltaXnames <- c(paste(rep(colnames(data), p), "t", -rep(1:p, 
                                                           each = k)))
  Bcolnames <- c("ECT", switch(include, const = "Const", trend = "Trend", 
                               both = c("Const", "Trend"), none = NULL), DeltaXnames)
  Blist <- nameB(Bbest, commonInter = ifelse(model == "All", 
                                             FALSE, TRUE), Bnames = Bcolnames, nthresh = nthresh, 
                 npar = npar, model = "TVECM", TVECMmodel = model)
  BnamesVec <- if (class(Blist) == "list") 
    c(sapply(Blist, colnames))
  else colnames(Blist)
  colnames(Bbest) <- BnamesVec
  naX <- rbind(matrix(NA, ncol = ncol(Zbest), nrow = p + 1), 
               Zbest)
  YnaX <- cbind(data, naX)
  BlistMod <- nameB(Bbest, commonInter = ifelse(model == "All", 
                                                FALSE, TRUE), Bnames = Bcolnames, nthresh = nthresh, 
                    npar = npar, model = "TVECM", TVECMmodel = model, sameName = FALSE)
  BnamesVecMod <- if (class(BlistMod) == "list") 
    c(sapply(BlistMod, colnames))
  else colnames(BlistMod)
  colnames(YnaX) <- c(colnames(data), BnamesVecMod)
  if (nthresh == 1) 
    nobs <- c(ndown = ndown, nup = nup)
  else if (nthresh == 2) 
    nobs <- c(ndown = ndown, nmiddle = 1 - nup - ndown, nup = nup)
  specific <- list()
  specific$Thresh <- bestThresh
  specific$threshEstim <- ifelse(is.null(gamma1), TRUE, FALSE)
  specific$nthresh <- nthresh
  specific$nreg <- nthresh + 1
  specific$beta <- bestBeta
  specific$coint <- matrix(c(1, -bestBeta), nrow = k, dimnames = list(colnames(data), 
                                                                      "r1"))
  specific$nrowB <- npar
  specific$nobs <- nobs
  specific$model <- model
  specific$oneMatrix <- ifelse(model == "only_ECT", TRUE, FALSE)
  specific$Bnames <- Bcolnames
  specific$regime <- regime
  specific$timeAttributes <- attributes(data[, 1])
  specific$LRinclude <- "none"
  specific$r <- 1
  z <- list(coefficients = Blist, residuals = resbest, model = YnaX, 
            coeffmat = Bbest, nobs_regimes = nobs, k = k, t = t, 
            T = T, nparB = allpar, fitted.values = fitted, lag = lag, 
            include = include, model.specific = specific)
  class(z) <- c("TVECM", "nlVar")
  attr(z, "varsLevel") <- "diff"
  attr(z, "model") <- model
  return(z)
}
<environment: namespace:tsDyn>
  
  