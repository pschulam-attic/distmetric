smoothing_matrix <- function(D, h) {
  L <- exp(- D / h)
  L <- t(apply(L, 1, function(r) r / sum(r)))
}

smoothing_loocv <- function(D, Y, h) {
  L <- smoothing_matrix(D, h)
  
  diff <- Y - (L %*% Y)
  denom <- 1 - diag(L)
  
  mean((diff / denom)^2)
}

mahal_dist <- function(X, A) {
  n <- nrow(X)
  D <- matrix(0, nrow=n, ncol=n)

  for (i in 1:n) {
    mdist <- mahalanobis(X[-i, ], center=as.numeric(X[i, ]), cov=A, inverted=TRUE)
    D[i, -i] <- mdist
  }

  sqrt(D)
}

nadarayawatson <- function(X, Y, disttype="euclidean", A) {
  distopts <- c("euclidean")
  mahal <- c("mahalanobis")

  if (disttype %in% distopts) {
    D <- as.matrix(dist(X, method=disttype))
  } else if (disttype %in% mahal) {
    D <- mahal_dist(X, A)
  } else {
    stop(sprintf("Invalid distance option: %s", disttype))
  }

  h <- c(0.01, 0.1, 1:100)
  loocvs <- sapply(h, function(h) smoothing_loocv(D, Y, h))
  
  L <- smoothing_matrix(D, h[order(loocvs)][1])

  structure(list(X=X, Y=Y, h=h, loocvs=loocvs, L=L), class="nadarayawatson")
}

loocv_mse <- function(nw) {
  lcv <- nw$loocvs
  lcv <- lcv[!is.nan(lcv)]
  min(lcv, na.rm=TRUE)
}
