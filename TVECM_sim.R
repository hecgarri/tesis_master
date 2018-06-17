TVECM.sim
function (data, B, TVECMobject, nthresh = 1, Thresh, beta, n = 200, 
          lag = 1, type = c("simul", "boot", "check"), 
          include = c("const", 
            "trend", "none", "both"), starting = NULL, innov = rmnorm(n, 
            varcov = varcov), varcov = diag(1, k), show.parMat = FALSE, 
          seed) 
{
  if (!missing(data) & !missing(B)) 
    stop("You have to provide either B or y, but not both")
  p <- lag
  type <- match.arg(type)
  include <- match.arg(include)
  isMissingB <- missing(B)
  if (!nthresh %in% c(0, 1, 2)) 
    stop("Arg nthresh should  be either 0, 1 or 2")
  if (!missing(n) & any(!missing(data), !missing(TVECMobject))) 
    stop("arg n should not be given with arg data or TVECMobject")
  if (!missing(TVECMobject) & any(!missing(Thresh), !missing(nthresh), 
                                  !missing(lag))) 
    warning("When object TVECMobject is given, only args 'type' and 'round' are relevant, others are not considered")
  ninc <- switch(include, none = 0, const = 1, trend = 1, both = 2)
  incVal <- switch(include, none = NULL, const = "const", trend = "trend", 
                   both = c("const", "trend"))
  if (!missing(B)) {
    if (missing(beta)) 
      stop("please provide arg beta (cointegrating value)")
    if (type != "simul") {
      type <- "simul"
      warning("Type check or boot are only avalaible with pre specified data. The type simul was used")
    }
    nB <- nrow(B)
    if (nB == 1) 
      stop("B matrix should at least have two rows for two variables\n")
    ndig <- 4
    esp <- p * nB + 1 + ninc
    pa <- switch(as.character(nthresh), `0` = "", `1` = c("_low", 
                                                          "_upr"), `2` = c("_low", "_mid", "_upr"))
    lags <- as.vector(outer("L{x", 1:nB, paste, sep = ""))
    lags2 <- paste(rep(lags, times = p), "}{", rep(1:p, each = p), 
                   "}", sep = "")
    if (esp * (nthresh + 1) != ncol(B)) {
      colnames_Matrix_input <- as.vector(outer(c("ECT", 
                                                 incVal, lags2), pa, paste, sep = ""))
      cat("Matrix B badly specified: should have ", esp * 
            (nthresh + 1), "columns, but has", ncol(B), "\n")
      print(matrix(NA, nrow = nB, ncol = esp * (nthresh + 
                                                  1), dimnames = list(paste("Equ x", 1:nB, sep = ""), 
                                                                      colnames_Matrix_input)))
      stop()
    }
    rownames(B) <- paste("Equ x", 1:nB, ":", sep = "")
    y <- matrix(0, ncol = nB, nrow = n)
    if (!is.null(starting)) {
      if (all(dim(as.matrix(starting)) == c(nB, p))) 
        y[seq_len(p), ] <- starting
      else stop("Bad specification of starting values. Should have nrow = lag and ncol = number of variables")
    }
    Bmat <- B
    k <- ncol(y)
    T <- nrow(y)
    if (is.vector(beta)) {
      if (length(beta) == k - 1) 
        beta <- c(1, -beta)
      tBETA <- matrix(beta, nrow = 1)
      r <- 1
    }
    else {
      if (nrow(beta) != k) 
        stop("beta should have k rows and r cols")
      r <- ncol(beta)
      tBETA <- t(beta)
    }
  }
  else if (!missing(data)) {
    if (nthresh == 0) {
      TVECMobject <- lineVar(data, lag = p, include = include, 
                             model = "VECM")
    }
    else {
      if (!missing(Thresh)) {
        if (nthresh == 1) {
          TVECMobject <- TVECM(data, lag = p, include = include, 
                               nthresh = nthresh, plot = FALSE, trace = FALSE, 
                               th1 = list(exact = Thresh))
        }
        else if (nthresh == 2) {
          TVECMobject <- TVECM(data, lag = p, include = include, 
                               nthresh = nthresh, plot = FALSE, trace = FALSE, 
                               th1 = list(exact = Thresh[1]), th2 = list(exact = Thresh[2]))
        }
      }
      else {
        TVECMobject <- TVECM(data, lag = p, include = include, 
                             nthresh = nthresh, plot = FALSE, trace = FALSE)
      }
    }
  }
  if (!missing(TVECMobject)) {
    k <- TVECMobject$k
    T <- TVECMobject$T
    p <- TVECMobject$lag
    include <- TVECMobject$include
    if (include %in% c("trend", "both")) 
      warning(paste("Accuracy of function (tested with arg type=check) is not good when arg include=", 
                    include, " is given\n"))
    modSpe <- TVECMobject$model.specific
    LRinclude <- modSpe$LRinclude
    nthresh <- modSpe$nthresh
    if (nthresh > 0 && modSpe$model == "only_ECT") 
      stop("TVECM.sim() does not work for 'common=only_ECT'")
    if (LRinclude != "none") 
      stop("TVECM.sim() does not work for 'LRinclude!='none'")
    beta <- -modSpe$coint[2, 1]
    tBETA <- t(modSpe$coint)
    r <- modSpe$r
    res <- residuals(TVECMobject)
    Bmat <- coefMat(TVECMobject)
    y <- as.matrix(TVECMobject$model)[, 1:k]
    ndig <- getndp(y[, 1])
    if (nthresh > 0) {
      Thresh <- modSpe$Thresh
      nthresh <- modSpe$nthresh
    }
  }
  t <- T - p - 1
  npar <- k * (p + ninc + 1)
  if (include != "both") {
    aa1 <- r + switch(include, none = 1:2, const = 2, trend = 1, 
                      both = NULL)
    aa <- sort(rep(aa1, each = nthresh + 1) + (0:nthresh) * 
                 (p * k + max(aa1)))
    Bmat <- myInsertCol(Bmat, c = aa, 0)
  }
  nparBmat <- p * k + 2 + 1
  Yb <- matrix(0, nrow = nrow(y), ncol = k)
  Yb[1:(p + 1), ] <- y[1:(p + 1), ]
  trend <- c(rep(NA, T - t), 1:t)
  if (type == "simul" && dim(innov) != c(n, k)) 
    stop(paste("input innov is not of right dim, should be matrix with", 
               n, "rows and ", k, "cols\n"))
  if (!missing(seed)) 
    set.seed(seed)
  resids <- switch(type, boot = res[sample(seq_len(t), replace = TRUE), 
                                    ], simul = innov, check = res)
  resb <- rbind(matrix(0, nrow = p + 1, ncol = k), resids)
  if (nthresh == 0) {
    for (i in (p + 2):T) {
      ECT <- Bmat[, 1:r] %*% tBETA %*% matrix(Yb[i - 1, 
                                                 ], ncol = 1)
      Yb[i, ] <- rowSums(cbind(Yb[i - 1, ], Bmat[, r + 
                                                   1], Bmat[, r + 2] * trend[i], ECT, Bmat[, -c(1:(r + 
                                                                                                     2))] %*% matrix(t(Yb[i - c(1:p), ] - Yb[i - c(2:(p + 
                                                                                                                                                        1)), ]), ncol = 1), resb[i, ]))
    }
  }
  else if (nthresh == 1) {
    BD <- Bmat[, seq_len(nparBmat)]
    BU <- Bmat[, -seq_len(nparBmat)]
    for (i in (p + 2):(nrow(y))) {
      ECT <- tBETA %*% matrix(Yb[i - 1, ], ncol = 1)
      if (round(ECT, ndig) <= Thresh) {
        Yb[i, ] <- rowSums(cbind(Yb[i - 1, ], BD[, 1] %*% 
                                   ECT, BD[, 2], BD[, 3] * trend[i], BD[, -c(1, 
                                                                             2, 3)] %*% matrix(t(Yb[i - c(1:p), ] - Yb[i - 
                                                                                                                         c(2:(p + 1)), ]), ncol = 1), resb[i, ]))
      }
      else {
        Yb[i, ] <- rowSums(cbind(Yb[i - 1, ], BU[, 1] %*% 
                                   ECT, BU[, 2], BU[, 3] * trend[i], BU[, -c(1, 
                                                                             2, 3)] %*% matrix(t(Yb[i - c(1:p), ] - Yb[i - 
                                                                                                                         c(2:(p + 1)), ]), ncol = 1), resb[i, ]))
      }
    }
  }
  else if (nthresh == 2) {
    BD <- Bmat[, seq_len(nparBmat)]
    BM <- Bmat[, seq_len(nparBmat) + nparBmat]
    BU <- Bmat[, seq_len(nparBmat) + 2 * nparBmat]
    for (i in (p + 2):(nrow(y))) {
      ECT <- tBETA %*% matrix(Yb[i - 1, ], ncol = 1)
      if (round(ECT, ndig) <= Thresh[1]) {
        Yb[i, ] <- rowSums(cbind(Yb[i - 1, ], BD[, 1] %*% 
                                   ECT, BD[, 2], BD[, 3] * trend[i], BD[, -c(1, 
                                                                             2, 3)] %*% matrix(t(Yb[i - c(1:p), ] - Yb[i - 
                                                                                                                         c(2:(p + 1)), ]), ncol = 1), resb[i, ]))
      }
      else if (round(ECT, ndig) > Thresh[2]) {
        Yb[i, ] <- rowSums(cbind(Yb[i - 1, ], BU[, 1] %*% 
                                   ECT, BU[, 2], BU[, 3] * trend[i], BU[, -c(1, 
                                                                             2, 3)] %*% matrix(t(Yb[i - c(1:p), ] - Yb[i - 
                                                                                                                         c(2:(p + 1)), ]), ncol = 1), resb[i, ]))
      }
      else {
        Yb[i, ] <- rowSums(cbind(Yb[i - 1, ], BM[, 1] %*% 
                                   ECT, BM[, 2], BM[, 3] * trend[i], BM[, -c(1, 
                                                                             2, 3)] %*% matrix(t(Yb[i - c(1:p), ] - Yb[i - 
                                                                                                                         c(2:(p + 1)), ]), ncol = 1), resb[i, ]))
      }
    }
  }
  if (show.parMat) {
    if (!isMissingB) {
      colnames_Matrix_system <- as.vector(outer(c("ECT", 
                                                  "Const", "Trend", lags2), pa, paste, sep = ""))
      colnames(Bmat) <- colnames_Matrix_system
    }
    else if (include != "both") {
      add <- switch(include, const = "Trend", trend = "Const", 
                    none = c("Const", "Trend"))
      colnames(Bmat)[aa] <- rep(add, nthresh + 1)
    }
    print(Bmat)
  }
  res <- round(Yb, ndig)
  return(res, resids)
}
<bytecode: 0x8ff1778>