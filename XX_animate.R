# do some fancy animation nonsense

library(ggplot2)
library(animation)

source("support_scripts/prepare_preds.R")
load("RData/0_format_aux_data.RData")



## animation within time period

# get one prediction grid to work from
pred_file <- dir("data/roms_grids", full.names = TRUE)[1]
predgrid <- prepare_preds(pred_file, pred_areas=pred_areas, poly=cce_poly)


# get a list of all the output files
out_pred_files <- dir("out", pattern="_pred.csv", full.names = TRUE)

out_pred_file <- out_pred_files[1]
#for(out_pred_file in out_pred_files){

# read-in and calculate the appropriate summaries
# **per time period**
this_pred <- read.table(out_pred_file, header=TRUE, sep=",")

do_it <- function(){
  for(i in 1:ncol(this_pred)){
    predgrid$N <- cut(this_pred[,i], c(0,0.001,.0023,.0036,.0085,.036,1, 10000))


    p_pred <- ggplot(predgrid, aes(y=mlat, x=mlon)) +
      geom_tile(aes(fill=N)) +
      scale_fill_viridis_d() +
      labs(x="", y="", fill="Density") +
      coord_quickmap() +
      theme_minimal() +
      theme(legend.position="none")
    print(p_pred)

  }
}


ani.options(interval = 0.15)
saveGIF(do_it(), outdir = "new", movie.name="within.gif")




## animation between time period

do_it <- function(){
  for(out_pred_file in out_pred_files){
    this_pred <- read.table(out_pred_file, header=TRUE, sep=",")

    predgrid$N <- cut(this_pred[,1], c(0,0.001,.0023,.0036,.0085,.036,1, 10000))


    p_pred <- ggplot(predgrid, aes(y=mlat, x=mlon)) +
      geom_tile(aes(fill=N)) +
      scale_fill_viridis_d() +
      labs(x="", y="", fill="Density") +
      coord_quickmap() +
      theme_minimal() +
      theme(legend.position="none")
    print(p_pred)

  }
}


ani.options(interval = 0.25)
saveGIF(do_it(), outdir = "new", movie.name="between.gif")







