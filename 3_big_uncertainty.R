# actually do the uncertainty bit
# this happens in a the function below that consists of 2 loops
# 1. (outer) over the prediction grids
# 2. (inner) model posterior simulations

# the action happens after this function declaration, scroll down :)

# function to calculate the uncertainty
#  vp           model (with variance propagated)
#  pred_files   vector of file paths for prediction grids
#  pred_areas   data.frame with areas of prediction cells
#  n_sims       number of simulations to do per pred grid
#  poly         polygon to bound the predictions
#  na_oob       remove out of bounds values (NA values outside model range)
#  quiet        should the function tell you what it's doing?
big_uncertainty <- function(vp, pred_files, pred_areas, n_sims, poly, na_oob=FALSE, quiet=FALSE){
  # extract model bits
  # coefficients
  betas <- coef(vp)
  # variance-covariance matrix,
  # corrected for smoothing parameter uncertainty
  Vb <- vcov(vp, unconditional = TRUE)

  if(na_oob){
    # get bounds
    covar_bnds <- get_covar_bnds(vp)
  }
  
  # storage for abundance estimates
  Nhat <- matrix(NA, nrow=length(pred_files), ncol=n_sims)

  # day counter
  dayi <- 1

  if(!quiet) cat("\n\n")
  # loop over the time periods
  for(pred_file in pred_files){

    if(!quiet) cat(pred_file, "\n")
    # prepare the prediction grid
    predgrid <- prepare_preds(pred_file, cce_poly, pred_areas, covar_bnds)
    # add in the random effects bits to the prediction grid
    predgrid[["XX"]] <- matrix(0, nrow(predgrid),
                               sum(grepl("XX", names(coef(vp)))))

    # storage for this time period
    out <- matrix(NA, nrow=nrow(predgrid), ncol=n_sims)

    # generate design matrix for predictions
    Xp <- predict(vp, predgrid, type="lpmatrix")

    # output file will have the same name as input but
    # end in _outs.csv instead
    outfile <- sub(".csv", "", pred_file)
    outfile <- sub("data/roms_grids/", "", outfile)
    outfile <- paste0("out/", outfile, "_pred.csv")

    # loop sampling from the posterior
    if(!quiet) pb <- txtProgressBar(0, n_sims, char=".")
    for(i in 1:n_sims){
      # generate new coefficients
      br <- rmvn(1, betas, Vb)
      # make a prediction and store it
      out[,i] <- exp(Xp%*%br)

      # calculate Nhat for this sim/day
      Nhat[dayi, i] <- sum(out[,i] * predgrid$area, na.rm=TRUE)
      if(!quiet) setTxtProgressBar(pb, i)
    }
    if(!quiet) cat("\n")
    # write out this time period's predictions as a CSV
    write.table(file=outfile, out, row.names = FALSE, sep=",", col.names=TRUE)

    dayi <- dayi + 1
  } # end loop over time periods

  # save the abundance estimates
  write.csv(Nhat, file="out/Nhat_ests.csv")

  return(TRUE)
}


library(mgcv)

# get the data from previous stages
load("RData/0_format_aux_data.RData")
load("RData/1_model_and_data.RData")
load("RData/2_prop_that_var.RData")
# load prepare_preds to get the grids in order
source("support_scripts/prepare_preds.R")
source("support_scripts/get_covar_bnds.R")

# get the paths to the grids
pred_files <- dir("data/roms_grids", full.names = TRUE)

# how many posterior samples?
n_sims <- 200


# run every third day...
#day_ind <- seq(1, length(pred_files), by=3)
#pred_files <- pred_files[day_ind]

# start timer
start_time <- Sys.time()
# do the work!
# things get saved to csv files in various places
big_uncertainty(b_vp, pred_files, pred_areas, n_sims, cce_poly, na_oob=TRUE, quiet=FALSE)


# end timer
end_time <- Sys.time()

print(end_time - start_time)