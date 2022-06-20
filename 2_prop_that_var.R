# now let's deal with the detection function uncertainty
# using the variance propagation method from Bravington, Miller and Hedley

library(Distance)
library(dsm)

load("RData/1_model_and_data.RData")

# use get_varprop_model to give us the refitted model that we want
# could use dsm::dsm_varprop here but this version is simpler
# and faster
source("support_scripts/get_varprop_model.R")
b_vp <- get_varprop_model(b, g0=g0, trace=TRUE, var_type="Vc")

# can save the checker info, but it's a mess
# because there are so many covariate combinations
checker_obj <- list(refit=b_vp, old_model=b)
vp_summ <- dsm:::varprop_check(checker_obj)
# fix this for data we have in spatial model
#> unique(fin$Ship)
#[1] MAC DSJ Mc2
vp_summ <- subset(vp_summ, Ship %in% c("MAC", "DSJ", "Mc2"))
vp_summ <- subset(vp_summ, SppMax=="074")
vp_summ <- unique(vp_summ)


# generate the random betas here and save for later

# number of simulations
n_sims <- 1000

# three options:

# 1. multivariate normal assumption
# (can work for well behaved models)
#beta_sims <- rmvn(n_sims, coef(b_vp), vcov(b_vp, unconditional = TRUE))

# 2. importance sampling weights
# (better for bad models, can still have problems)
#source("support_scripts/likelihood_tools.R")
#source("support_scripts/ttools.R")
#source("support_scripts/importance.R")
#beta_sims <- gam.imp(ns=n_sims, b_vp)
#
## need to save the weights too!
#imp_weights <- beta_sims$wts
#beta_sims <- beta_sims$br

# 3. Metropolis-Hastings
# (should work but requires some fiddling with rw.scale parameters to ensure
#  that acceptance rate is good)
source("support_scripts/likelihood_tools.R")
source("support_scripts/ttools.R")
set.seed(1234)
beta_sims <- gam.mh(b_vp, ns=n_sims, burn=2000, rw.scale=0.05)$bs

# save the varprop'd model
save(b_vp, beta_sims, file="RData/2_prop_that_var.RData")
