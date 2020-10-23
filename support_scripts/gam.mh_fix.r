## Simple post fit mcmc for mgcv.
## (c) Simon N. Wood (2020)

# mgcv::gam.mh with fix via Len Thomas

gam.mh <- function(b,ns=10000,burn=1000,t.df=40,rw.scale=.25,thin=1) {
## generate posterior samples for fitted gam using Metroplois Hastings sampler
## alternating fixed proposal and random walk proposal, both based on Gaussian
## approximation to posterior...
  if (inherits(b,"bam")) stop("not usable with bam fits")
  beta <- coef(b);Vb <- vcov(b)
  X <- model.matrix(b); burn <- max(0,burn)
  prog <- interactive();iprog <- 0
  di <- floor((ns+burn)/100)
  if (prog) prg <- txtProgressBar(min = 0, max = ns+burn, initial = 0,
                   char = "=",width = NA, title="Progress", style = 3)
  bp <- rmvt(ns+burn,beta,Vb,df=t.df) ## beta proposals
  bp[1,] <- beta ## Don't change this after density step!!
  lfp <- dmvt(t(bp),beta,Vb,df=t.df) ## log proposal density

  rw <- is.finite(rw.scale)&&rw.scale>0
  if (rw) {
    R <- chol(Vb) 
    step <- rmvn(ns+burn,beta*0,Vb*rw.scale) ## random walk steps (mgcv::rmvn)
  }
  u <- runif(ns+burn);us <- runif(ns+burn) ## for acceptance check
  bs <- bp;j <- 1;accept <- rw.accept <- 0
  lpl0 <- lpl(b,bs[1,],X)
  for (i in 2:(ns+burn)) { ## MH loop
    ## first a static proposal...
    lpl1 <- lpl(b,bs[i,],X)
    if (u[i] < exp(lfp[j]-lfp[i]+lpl1-lpl0)) {
      lpl0 <- lpl1;accept <- accept + 1
      j <- i ## row of bs containing last accepted beta
    } else bs[i,] <- bs[i-1,]
    ## now a random walk proposal...
    if (rw) {
      lpl1 <- lpl(b,bs[i,]+step[i,],X)
      if (us[i] < exp(lpl1-lpl0)) { ## accept random walk step
        lpl0 <- lpl1;j <- i
        bs[i,] <- bs[i,] + step[i,]
	rw.accept <- rw.accept+1 
        #lfp[i] <- dmvt(bs[i,],beta,Vb,df=4,R=R) ## have to update static proposal density
        # FIX via LJT 5/10/20
        lfp[i] <- dmvt(bs[i,],beta,Vb,df=t.df,R=R) 
      }
    }  
    if (i==burn) accept <- rw.accept <- 0
    if (prog&&i%%di==0) setTxtProgressBar(prg, i)
  } ## MH loop
  if (burn>0) bs <- bs[-(1:burn),]
  if (thin>1) bs <- bs[seq(1,ns,by=thin),]
  if (prog) {
    setTxtProgressBar(prg, i);cat("\n")
    cat("fixed acceptance = ",accept/ns,"  RW acceptance = ",rw.accept/ns,"\n")
  }  
  list(bs=bs,rw.accept = rw.accept/ns,accept=accept/ns)
} ## gam.mh

