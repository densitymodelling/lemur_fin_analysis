# process all those outputs

# take each file and calulate the running mean and variance using the method
# of Welford
# https://en.wikipedia.org/wiki/Algorithms_for_calculating_variance#Welford's_online_algorithm

source("support_scripts/prepare_preds.R")
load("RData/0_format_aux_data.RData")
load("RData/1_model_and_data.RData")

# get one prediction grid to work from
pred_file <- dir("data/roms_grids", full.names = TRUE)[1]
summary_predgrid <- prepare_preds(pred_file, pred_areas=pred_areas, poly=cce_poly)

# function to calculate variance using Welford's method
# reads in each file named in out_files
welford_mean_var <- function(out_files, summary_predgrid){
  # setup for calculations for Welford's variance estimation
  curr_mean <- rep(0, nrow(summary_predgrid))
  M2 <- rep(0, nrow(summary_predgrid))
  count <- rep(0, nrow(summary_predgrid))

  # loop over time periods
  for(out_file in out_files){

    # read-in and calculate the appropriate summaries
    this_pred <- readRDS(out_file)

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

  return(summary_predgrid)
}


## first calculate over all time periods
# get a list of all the output files
all_out_files <- dir("out", pattern="_pred.rds", full.names = TRUE)

# calculate mean and variance
summary_predgrid_all <- welford_mean_var(all_out_files, summary_predgrid)

# roll in the g0 CV per cell
summary_predgrid_all$sdd_g0 <- sqrt((summary_predgrid_all$sdd/
                                     summary_predgrid_all$avv)^2 +
                                    g0_CV^2)*summary_predgrid_all$avv

## now calculate for each year
summary_predgrid_yearly <- c()
for(year in c(1996, 2001, 2005, 2008, 2014)){

  # get a list of all the output files
  year_out_files <- dir("out", pattern=paste0("CCE_0.1deg_", year,"-"),
                        full.names = TRUE)

  # calculate mean and variance
  summary_predgrid_tmp <- welford_mean_var(year_out_files, summary_predgrid)
  # add in year column
  summary_predgrid_tmp$year <- year

  # rbind onto a bigger data.frame
  summary_predgrid_yearly <- rbind(summary_predgrid_yearly,
                                   summary_predgrid_tmp)
}

# roll in the g0 CV per cell using the delta method
summary_predgrid_yearly$sdd_g0 <- sqrt((summary_predgrid_yearly$sdd/
                                     summary_predgrid_yearly$avv)^2 +
                                    g0_CV^2)*summary_predgrid_yearly$avv

# save that output
save(summary_predgrid_all,
     summary_predgrid_yearly,
     file="RData/4_process_uncertainty.RData")
