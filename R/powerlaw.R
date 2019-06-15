#' Get parameters for powerlaw fit to community ISD
#'
#' @param dat community df with `individual_sizes` column
#' @param estimate_min whether to estimate the minimum for the power law, or use the minimum of `individual_sizes`. Defaults TRUE for estimate.
#' @return list of xmin and alpha
#' @export
get_pl_pars <- function(dat, estimate_min = TRUE) {

  library(poweRlaw)

  m = conpl$new(dat$individual_sizes)
  if(estimate_min) {
    minimum_x = estimate_xmin(m)$xmin
  } else {
    minimum_x = min(dat$individual_sizes)
  }
  m$setXmin(minimum_x)
  estimated_alpha = estimate_pars(m)$pars

  plpars <- list(xmin = minimum_x,
                 alpha = estimated_alpha)

  return(plpars)
}

#' Get loglik of empirical from powerlaw
#' @param dat community dataframe with individual_sizes column
#' @param plpars result of get_pl_pars(dat)
#' @return loglikelihood
#' @export
#' @importFrom poweRlaw dplcon
loglik_pl <- function(dat, plpars) {
  summed_loglik <- sum(poweRlaw::dplcon(dat$individual_sizes, xmin = plpars$xmin, alpha = plpars$alpha, log = TRUE))

  return(summed_loglik)
}
