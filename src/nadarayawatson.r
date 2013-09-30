nwdist <- function(X, method, A) {
  
  mdist <- function(i) {
    d <- rep(0, nrow(X))
    d[-i] <- mahalanobis(X[-i, ], center=as.numeric(X[i, ]), cov=A, inverted=TRUE)
    sqrt(d)
  }
  
  if (method == "mahalanobis") {
    t(sapply(1:nrow(X), mdist))
  } else {
    as.matrix(dist(X, method=method))
  }
}

nadarayawatson <- function(X, Y, dmethod="euclidean", A=NULL) {
  D <- nwdist(X, dmethod, A)

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
      loocvmse = loo_mse[order(loo_mse)][1],
      dmethod = dmethod,
      A = A
      ), class = "nadarayawatson")
}

predict.nadarayawatson <- function(nwfit, newdata=NULL, se=FALSE) {

  nwboot <- function(df, idx) {
    df <- df[idx, ]
    w <- exp(-df$d / nwfit$h)
    l <- w / sum(w)
    sum(l * df$Y)
  }

  make_pred <- function(newx, X, Y) {
    X <- rbind(newx, X)
    d <- nwdist(X, nwfit$dmethod, nwfit$A)[1, -1]
    w <- exp(-d / nwfit$h)
    l <- w / sum(w)

    p <- sum(l * Y)

    if (se) {
      require(boot)
      s <- boot(data.frame(d=d, Y=Y), nwboot, R=1000)
      data.frame(prediction=p, se=sd(s$t))
    } else {
      data.frame(prediction=p)
    }
  }

  n <- nrow(nwfit$X)
  preds <-
      do.call(rbind,
              lapply(1:n, function(i) {
                make_pred(X[i, ], X[-i, ], Y[-i])
              }))
  preds
}
