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

# save the varprop'd model
save(b_vp, file="RData/2_prop_that_var.RData")
