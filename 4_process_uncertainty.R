# process all those outputs!

source("support_scripts/prepare_preds.R")
load("RData/0_format_aux_data.RData")

# get one prediction grid to work from
pred_file <- dir("data/roms_grids", full.names = TRUE)[1]
summary_predgrid <- prepare_preds(pred_file, pred_areas=pred_areas, poly=cce_poly)


# get a list of all the output files
out_pred_files <- dir("out", pattern="_pred.csv", full.names = TRUE)

# storage, here we're going to get an average and the standard deviation
avg <- matrix(NA, nrow=nrow(summary_predgrid), ncol=length(out_pred_files))
sd <- matrix(NA, nrow=nrow(summary_predgrid), ncol=length(out_pred_files))

i<-1
for(out_pred_file in out_pred_files){

  # read-in and calculate the appropriate summaries
  # **per time period**
  this_pred <- read.table(out_pred_file, header=TRUE, sep=",")

  #this_pred <- apply(this_pred, 1, function(x){
  #  ul <- boxplot.stats(x)$stats[c(1,5)]
  #  x[x>ul[2]] <- NA
  #  x
  #})

  avg[,i] <- apply(this_pred, 1, median, na.rm=TRUE)
  sd[,i] <- apply(this_pred, 1, sd, na.rm=TRUE)

  i <- i + 1
}



# now get the summaries between time periods
summary_predgrid$avv <- apply(avg, 1, median, na.rm=TRUE)
summary_predgrid$sdd <- apply(sd, 1, function(x) sqrt(sum(x^2)))


save(summary_predgrid, file="RData/4_process_uncertainty.RData")
