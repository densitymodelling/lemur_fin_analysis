#' Variance propagation for density surface models
#'
#' Calculate the uncertainty in predictions from a fitted DSM, including uncertainty from the detection function.
#'
#' This function will refit the spatial model but include the Hessian of the offset as an extra term. Variance estimates using this new model can then be used to calculate the variance of predicted abundance estimates which incorporate detection function uncertainty. Importantly this requires that if the detection function has covariates, then these do not vary within a segment (so, for example covariates like sex cannot be used).
#'
#' This routine is only useful if a detection function with covariates has been used in the DSM.
#'
#' Note that we can use \code{var_type="Vc"} here (see \code{\link{gamObject}}), which is the variance-covariance matrix for the spatial model, corrected for smoothing parameter uncertainty. See Wood, Pya & S{\"a}fken (2016) for more information.
#'
#' @section Diagnostics:
#' The summary output from the function includes a simply diagnostic that shows the average probability of detection from the "original" fitted model (the model supplied to this function; column \code{Fitted.model}) and the probability of detection from the refitted model (used for variance propagation; column \code{Refitted.model}) along with the standard error of the probability of detection from the fitted model (\code{Fitted.model.se}), at the unique values of any factor covariates used in the detection function (for continuous covariates the 5%, 50% and 95% quantiles are shown). If there are large differences between the probabilities of detection then there are potentially problems with the fitted model, the variance propagation or both. This can be because the fitted model does not account for enough of the variability in the data and in refitting the variance model accounts for this in the random effect.
#'
#' @return refitted model object, with extra term
#' @author David L. Miller, based on code from Mark V. Bravington and Sharon L. Hedley.
#' @references
#' Williams, R., Hedley, S.L., Branch, T.A., Bravington, M.V., Zerbini, A.N. and Findlay, K.P. (2011). Chilean Blue Whales as a Case Study to Illustrate Methods to Estimate Abundance and Evaluate Conservation Status of Rare Species. Conservation Biology 25(3), 526-535.
#'
#' Wood, S.N., Pya, N. and S{\"a}fken, B. (2016) Smoothing parameter and model selection for general smooth models. Journal of the American Statistical Association, 1-45.
#'
#'
#' @param model a fitted \code{\link{dsm}}
#' @param g0 fixed g0 estimate, 1 if not used (uncertainty not taken into account)
#' @param trace for debugging, see how the scale parameter estimation is going
#' @param var_type which variance-covariance matrix should be used (\code{"Vp"} for variance-covariance conditional on smoothing parameter(s), \code{"Vc"} for unconditional). See \code{\link{gamObject}} for an details/explanation. If in doubt, stick with the default, \code{"Vp"}.
#' @export
#'
get_varprop_model <- function(model, g0=1, trace=FALSE, var_type="Vp"){

  # die if the link isn't log
  if(model$family$link != "log"){
    stop("log link must be used!")
  }

  # check for valid var_type
  if(!(var_type %in% c("Vp","Vc"))){
    stop("var_type must be \"Vp\" or \"Vc\"")
  }

  if(model$ddf$ds$aux$ddfobj$scale$formula=="~1"){
    stop("varprop doesn't work when there are no covariates in the detection function")
  }

  # extract the link & invlink
  linkfn <- model$family$linkfun
  linkinvfn <- model$family$linkinv

  # die if we're not using REML
  if(model$method != "REML"){
    stop("REML must be used for smoothing parameter selection")
  }

  # extract the call
  this_call <- as.list(model$call)
  # remove the function
  this_call[1] <- NULL

  # extract the detection function
  ddf <- model$ddf

  # function to differentiate
  mu_fn <- function(par, linkfn, ddf, data, ds_newdata, g0){
    # set the detection function parameters to be par
    ddf$par <- par

    # calculate mu (effective strip width)
    mu <- predict(ddf, newdata=ds_newdata, esw=TRUE, compute=TRUE)$fitted

    # repopulate with the duplicates back in
    mu <- mu[attr(u_ds_newdata, "index"), drop=FALSE]

    # calculate offset
    if(model$ddf$meta.data$point){
      # calculate log effective circle area
      # nb. predict() returns effective area of detection for points
      ret <- linkfn(mu * g0 * data$Effort)
    }else{
      # calculate log effective strip width
      ret <- linkfn(2 * mu * g0 * data$Effort)
    }


    return(ret)
  }

  # extract the formula
  ds_formula <- ddf$ds$aux$ddfobj$scale$formula

  # if we don't have covariates then just setup the
  # data for predict.ds to be a distance of 0, which will be
  # ignored anyway
  if(ds_formula=="~1"){
    ds_newdata <- data.frame(distance=rep(0, nrow(model$data)))
  }else{
    # otherwise need the covars that are in the data (that we saved
    # with keepData=TRUE :))
    ds_newdata <- model$data[, all.vars(as.formula(ds_formula)), drop=FALSE]
    ds_newdata$distance <- 0
  }

  # probably a lot of duplicated stuff in the above, so let's just
  # pass the unique combinations
  # inside mu_fn will return the right length
  u_ds_newdata <- mgcv::uniquecombs(ds_newdata)

  # find the derivatives of log(mu)
  firstD <- dsm:::numderiv(mu_fn, ddf$par, linkfn=linkfn, ddf=ddf,
                     data=model$data, ds_newdata=u_ds_newdata, g0=g0)
  if(!is.matrix(firstD)){
    firstD <- matrix(firstD, ncol=length(ddf$par))
  }


  # put that in the data
  dat <- model$data
  dat[["XX"]] <- firstD

  # do a clever thing above to ensure we don't clobber
  # some other variable name

  ## build the model
  # update the formula to include the new term
  this_call$formula <- update.formula(this_call$formula,
                         paste0(paste(as.character(this_call$formula)[c(2,1,3)],
                                 collapse=" "), " + XX"))
  # update data
  this_call$data <- dat
  # add paraPen bit
  # hessian needs to be 2nd REAL Hessian not DS thing
  # that lives in $hessian
  opt_details <- attr(ddf$ds,"details")
  if(is.matrix(opt_details)){
    hess <- opt_details[nrow(opt_details),]$nhatend
  }else{
    hess <- opt_details$nhatend
  }
  if(any(is.na(hess))){
    # fall back to DS use if things are bad
    hess <- ddf$hessian
  }
  this_call$paraPen <- c(this_call$paraPen, list(XX=list(hess)))
  # tell gam.fixed.priors what to look for
  this_call$fixed.priors <- "XX"

  # set the trace on the scale parameter estimation
  this_call$scale.trace <- trace
  
  # is the scale estimated for this model?
  this_call$scale.estimated <- model$scale.estimated

  ## refit the model
  gam.fixed.priors <- dsm:::gam.fixed.priors
  refit <- do.call("gam.fixed.priors", this_call)

  refit$data <- dat
  
  ## now do some predictions

  return(refit)
}
