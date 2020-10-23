# taken from mgcv source code

## Simple post fit mcmc for mgcv.
## (c) Simon N. Wood (2020)


## some functions to extract important components of joint density from
## fitted gam...

## evaluate penalty for fitted gam, possibly with new beta
# patched to include parapen
bSb <- function(b,beta=coef(b)) {
  bSb <- k <-  0
  sp <- if (is.null(b$full.sp)) b$sp else b$full.sp ## handling linked sp's


  # the parapen bits
  # need to do something clever with L at some point
  if(!is.null(b$paraPen)){
    for (i in 1:length(b$paraPen$S)) {
      k <- k + 1
      # get indices
      ii <- b$paraPen$off[i]
      ii <- ii:(ii+ncol(b$paraPen$S[[i]])-1)
      # add to penalty
      bSb <- bSb + sp[b$paraPen$full.sp.names[i]]*
                    (t(beta[ii])%*%b$paraPen$S[[i]]%*%beta[ii])
    }
  }


  for (i in 1:length(b$smooth)) {
    m <- length(b$smooth[[i]]$S)
    if (m) {
      ii <- b$smooth[[i]]$first.para:b$smooth[[i]]$last.para
      for (j in 1:m) {
        k <- k + 1
        bSb <- bSb + sp[k]*(t(beta[ii])%*%b$smooth[[i]]$S[[j]]%*%beta[ii])
      }
    }
  }


  bSb
} ## bSb

devg <- function(b,beta=coef(b),X=model.matrix(b)) {
## evaluate the deviance of a fitted gam given possibly new coefs, beta
## for general families this is simply -2*log.lik
  if (inherits(b$family,"general.family")) {
    -2*b$family$ll(b$y,X,beta,b$prior.weights,b$family,offset=b$offset)$l
  } else { ## exp or extended family
    sum(b$family$dev.resids(b$y,b$family$linkinv(X%*%beta+b$offset),b$prior.weights))
  }
} ## devg

lpl <- function(b,beta=coef(b),X=model.matrix(b)) {
## log joint density for beta, to within uninteresting constants
  -(devg(b,beta,X)/b$sig2+bSb(b,beta)/b$sig2)/2
}

