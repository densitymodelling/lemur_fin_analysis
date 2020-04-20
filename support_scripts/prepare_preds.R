# prepare a prediction grid by ensuring that:
#  1. it has the right column names
#  2. offset is 1
#  3. prediction cell area is in $area
#  4. all cells are inside poly
prepare_preds <- function(pred_file, poly, pred_areas, covar_bnds=NULL){
  # think about predictions!
  predgrid <- read.csv(pred_file)
  # rename the columns
  names(predgrid) <- c("mlat", "mlon", "sst", "sst_sd", "ssh", "ssh_sd", "ild", "ild_sd")

  # set the offset
  predgrid$off <- 1
  # set the area
  predgrid$area <- pred_areas$pixelkm2
  
  # set the year from the file name
  year <- sub("data/roms_grids/CCE_0.1deg_", "", pred_file)
  year <- sub("-\\d{2}-\\d{2}.csv", "", year)
  predgrid$year <- as.numeric(year)
  
  # need to set x and y to get the following to work
  predgrid$x <- predgrid$mlon
  predgrid$y <- predgrid$mlat
  # clip prediction grid to CCE
  ind <- splancs::inout(predgrid[,c("x", "y")], poly, bound=TRUE)
  predgrid <- predgrid[ind,]
  # remove x, y
  predgrid$x <- NULL
  predgrid$y <- NULL
  
  # set as NA grid cells that have covariate values outside of the
  # bounds in covar_bnds
  if(!is.null(covar_bnds)){
    for(covar in names(predgrid)){
      this_bnd <- covar_bnds[[covar]]
      predgrid[[covar]][predgrid[[covar]] < this_bnd[1] |
                        predgrid[[covar]] > this_bnd[2] ] <- NA
    }  
  }
  
  return(predgrid)
}