# process all those outputs!

# using data.table::fread to make file reading faster
library(data.table)


source("support_scripts/prepare_preds.R")
load("RData/0_format_aux_data.RData")
load("RData/1_model_and_data.RData")

# get one prediction grid to work from
pred_file <- dir("data/roms_grids", full.names = TRUE)[1]
summary_predgrid <- prepare_preds(pred_file, pred_areas=pred_areas, poly=cce_poly)


# get a list of all the output files
out_pred_files <- dir("out", pattern="_pred.csv", full.names = TRUE)

# setup for calculations for Welford's variance estimation
curr_mean <- rep(0, nrow(summary_predgrid))
M2 <- rep(0, nrow(summary_predgrid))
count <- rep(0, nrow(summary_predgrid))

# loop over time periods
for(out_pred_file in out_pred_files){

  # read-in and calculate the appropriate summaries
  this_pred <- data.table::fread(out_pred_file, header=TRUE, sep=",")

  this_pred <- as.data.frame(this_pred)
  # running variance calculation a la Welford
  # loop over sims
  for(ii in 1:ncol(this_pred)){
    this_sim <- this_pred[,ii]
    # how many samples go into this summary? Exclude NA
    count <- count + !is.na(this_sim)
    # remove NAs for the rest of the calculation
    na_ind <- is.na(this_sim)
    this_sim[na_ind] <- 0
    
    # this implements Welford's method
    delta <- this_sim - curr_mean
    delta[na_ind] <- 0
    dc <- delta/count
    dc[na_ind] <- 0
    curr_mean <- curr_mean + dc
    delta2 <- this_sim - curr_mean
    delta2[na_ind] <- 0
    M2 <- M2 + delta*delta2
  }
}

# extract the mean and sample standard deviation
summary_predgrid$avv <- curr_mean
summary_predgrid$sdd <- sqrt(M2/(pmax(0, count-1)))

# roll in the g0 CV
summary_predgrid$sdd_g0 <- sqrt((summary_predgrid$sdd/summary_predgrid$avv)^2 + g0_CV^2)*summary_predgrid$avv


save(summary_predgrid, file="RData/4_process_uncertainty.RData")
