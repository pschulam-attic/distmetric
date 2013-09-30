dist2 <- function(X, method, A) {
  
  mdist <- function(i) {
    d <- rep(0, nrow(X))
    d[-i] <- mahalanobis(X[-i, ], center=as.numeric(X[i, ]), cov=A, inverted=TRUE)
    sqrt(d)
  }
  
  if (method == "mahalanobis") {
    t(sapply(1:nrow(X), mdist))
  } else {
    dist(X, method=method)
  }
}

nadarayawatson <- function(X, Y, D) {

  smoother <- function(h) {
    L <- exp(-D / h)
    t(apply(L, 1, function(r) r / sum(r)))
  }
  
  loocv <- function(h) {
    L <- smoother(h)
    mean( (Y - (L %*% Y))^2 / (1 - diag(L))^2 )
  }

  hs <- 1:100
  loo_mse <- sapply(hs, loocv)
  h <- hs[order(loo_mse)][1]

  L <- smoother(h)

  structure(list(
      X = X,
      Y = Y,
      D = D,
      h = h,
      L = L,
      loocvmse = loo_mse[order(loo_mse)][1]
      ), class = "nadarayawatson")
}

predict.nadarayawatson <- function(nwfit) {
  L <- nwfit$L
  Y <- nwfit$Y
  as.numeric(L %*% Y)
}

loocv_mse <- function(nw) {
  lcv <- nw$loocvs
  lcv <- lcv[!is.nan(lcv)]
  min(lcv, na.rm=TRUE)
}

stderr <- function(nwfit, nboot=500) {
  n <- length(nwfit$Y)
  means <- rep(0, n)
  stderrs <- rep(0, n)

  for (i in seq_len(n)) {
    bootfn <- function() {
      idx <- sample(n-1, n-1, replace=TRUE)
      d <- nwfit$D[i, -i][idx]
      w <- d / sum(d)
      y <- nwfit$Y[-i][idx]
      
      sum(w * y)
    }

    b <- replicate(nboot, bootfn())
    means[i] <- mean(b)
    stderrs[i] <- sd(b)
  }

  list(means=means, stderrs=stderrs)
}
