# now let's deal with the detection function uncertainty

library(Distance)
library(dsm)

load("RData/1_model_and_data.RData")

# make fake pred data
fake_pred_data <- b$data
fake_pred_data$count <- NULL

# use dsm_varprop1 to give us the refitted model that we want
source("support_scripts/dsm_varprop.R")
b_vp <- dsm_varprop1(b, fake_pred_data[1:20,], var_type="Vc")
# this just returns the model
b_vp$data <- b$data

# can save the checker info, but it's a mess
# because there are so many covariate combinations
checker_obj <- list(refit=b_vp, old_model=b)
vp_summ <- dsm:::varprop_check(checker_obj)
# fix this for min data we have in spatial model
#> unique(fin$Ship)
#[1] MAC DSJ Mc2
vp_summ <- subset(vp_summ, Ship %in% c("MAC", "DSJ", "Mc2"))
vp_summ <- subset(vp_summ, SppMax=="074")
#vp_summ <- subset(vp_summ, LnTotSS==unique(fin$LnTotSS))
vp_summ <- unique(vp_summ)

# save this
save(b_vp, file="RData/2_prop_that_var.RData")