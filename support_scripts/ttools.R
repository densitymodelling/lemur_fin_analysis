## t-distribution stuff for mgcv.
## (c) Simon N. Wood (2020)

## some useful densities (require mgcv::rmvn)...

rmvt <- function(n,mu,V,df) {
## simulate multivariate t variates  
  y <- rmvn(n,mu*0,V)
  v <- rchisq(n,df=df)
  t(mu + t(sqrt(df/v)*y))
}

r.mvt <- function(n,mu,V,df) rmvt(n,mu,V,df)

dmvt <- function(x,mu,V,df,R=NULL) {
## multivariate t log density...
  p <- length(mu);
  if (is.null(R)) R <- chol(V)
  z <- forwardsolve(t(R),x-mu)
  k <- - sum(log(diag(R))) - p*log(df*pi)/2 + lgamma((df+p)/2) - lgamma(df/2)
  k - if (is.matrix(z)) (df+p)*log1p(colSums(z^2)/df)/2 else (df+p)*log1p(sum(z^2)/df)/2
}

d.mvt <- function(x,mu,V,df,R=NULL) dmvt(x,mu,V,df,R)
