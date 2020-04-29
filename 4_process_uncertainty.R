# process all those outputs!

source("support_scripts/prepare_preds.R")
load("RData/0_format_aux_data.RData")
load("RData/1_model_and_data.RData")

# get one prediction grid to work from
pred_file <- dir("data/roms_grids", full.names = TRUE)[1]
summary_predgrid <- prepare_preds(pred_file, pred_areas=pred_areas, poly=cce_poly)


# get a list of all the output files
out_pred_files <- dir("out", pattern="_pred.csv", full.names = TRUE)

# storage, here we're going to get an average and the standard deviation
avg <- matrix(NA, nrow=nrow(summary_predgrid), ncol=length(out_pred_files))
sd <- matrix(NA, nrow=nrow(summary_predgrid), ncol=length(out_pred_files))

i<-1

# setup for calculations
curr_mean <- rep(0, 11860)
M2 <- rep(0, 11860)
count <- rep(0, 11860)

# loop over time periods
for(out_pred_file in out_pred_files){

  # read-in and calculate the appropriate summaries
  this_pred <- read.table(out_pred_file, header=TRUE, sep=",")

  for(ii in 1:ncol(this_pred)){
    this_sim <- this_pred[,ii]
# count += 1
    count <- count + !is.na(this_sim)
    # remove NAs for the rest of the calculation
    na_ind <- is.na(this_sim)
    this_sim[na_ind] <- 0
# delta = newValue - mean
    delta <- this_sim - curr_mean
    delta[na_ind] <- 0
# mean += delta / count
    dc <- delta/count
    dc[na_ind] <- 0
    curr_mean <- curr_mean + dc
# delta2 = newValue - mean
    delta2 <- this_sim - curr_mean
    delta2[na_ind] <- 0
# M2 += delta * delta2
    M2 <- M2 + delta*delta2
    
  }

#  avg[,i] <- apply(this_pred, 1, mean, na.rm=TRUE)
#  sd[,i] <- apply(this_pred, 1, sd, na.rm=TRUE)

  i <- i + 1
}

## now get the summaries between time periods
#summary_predgrid$avv <- apply(avg, 1, mean, na.rm=TRUE)
#summary_predgrid$sdd <- apply(sd, 1, function(x) sqrt(sum(x^2, na.rm=TRUE)))

#(mean, variance, sampleVariance) = (mean, M2 / count, M2 / (count - 1))
summary_predgrid$avv <- curr_mean
summary_predgrid$sdd <- sqrt(M2/(pmax(0, count-1)))

summary_predgrid$sdd_g0 <- sqrt((summary_predgrid$sdd/summary_predgrid$avv)^2 + g0_CV^2)*summary_predgrid$avv


save(summary_predgrid, file="RData/4_process_uncertainty.RData")

  #this_pred <- apply(this_pred, 1, function(x){
  #  ul <- boxplot.stats(x)$stats[c(1,5)]
  #  x[x>ul[2]] <- NA
  #  x
  #})