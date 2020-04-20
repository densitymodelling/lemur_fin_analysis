# get the bounds for the covariates in a model
get_covar_bnds <- function(model){
  if(is.null(model$data)){
    stop("No data in this model!")
  }
  
  dat <- model$data
  
  # get the max/min for all columns, ignoring factors
  ranges <- lapply(b$data, function(x){ if(!is.factor(x)) range(x, na.rm=TRUE)})
  # remove the NULL entries corresponding to the factors
  ranges <- Filter(function(x) !is.null(x), ranges)
  
  return(ranges)
}