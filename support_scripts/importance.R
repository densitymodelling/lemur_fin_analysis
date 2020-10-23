# importance sampling based on code from mgcv
gam.imp <- function(b, ns=10000){

  beta <- coef(b)
  Vb <- vcov(b)
  X <- model.matrix(b)

  # beta proposals
  bs <- rmvn(ns, beta, Vb)

  # log proposal density
  lfp <- dmvn(t(bs), beta, Vb)
  # log density for penalized MLE
  lfp1 <- dmvn(matrix(beta, ncol=1), beta, Vb)

  # storage
  wts <- rep(0, ns)

  # loglik for penalized MLE
  lpl0 <- lpl(b, beta, X)

  for (i in 1:ns) {
    # loglik of proposal...
    lpl1 <- lpl(b, bs[i,], X)
    # store weight
    wts[i] <- exp(lfp1-lfp[i]+lpl1-lpl0)
  }
  list(bs=bs, wts=wts)
}
