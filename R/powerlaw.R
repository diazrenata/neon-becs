#' Get parameters for powerlaw fit to community ISD
#'
#' @param dat community df with `individual_sizes` column
#' @param estimate_min whether to estimate the minimum for the power law, or use the minimum of `individual_sizes`. Defaults TRUE for estimate.
#' @param size_or_energy "size" or "energy", defaults "size"
#' @return list of xmin and alpha
#' @export
get_pl_pars <- function(dat, estimate_min = TRUE, size_or_energy = "size") {

  library(poweRlaw)
  if(size_or_energy == "size") {
    m = conpl$new(dat$individual_sizes)
    if(estimate_min) {
      minimum_x = estimate_xmin(m)$xmin
    } else {
      minimum_x = min(dat$individual_sizes)
    }
  } else if (size_or_energy == "energy") {
    m = conpl$new(dat$individual_energy)
    if(estimate_min) {
      minimum_x = estimate_xmin(m)$xmin
    } else {
      minimum_x = min(dat$individual_energy)
    }
  }
  m$setXmin(minimum_x)
  estimated_alpha = estimate_pars(m)$pars

  plpars <- list(xmin = minimum_x,
                 alpha = estimated_alpha)

  return(plpars)
}

#' Get loglik of empirical from powerlaw
#' @param values individual_sizes or individual_energy column
#' @param plpars result of get_pl_pars(dat)
#' @return loglikelihood
#' @export
#' @importFrom poweRlaw dplcon
loglik_pl <- function(values, plpars) {
    summed_loglik <- sum(poweRlaw::dplcon(values, xmin = plpars$xmin, alpha = plpars$alpha, log = TRUE))

  return(summed_loglik)
}

#' Sample from fitted powerlaw
#'
#' @param dat community dataframe with individual_sizes column
#' @param plpars result of get_pl_pars(dat)
#' @param nsamples how many to draw
#'
#' @return matrix of samples
#' @export
#'
#' @importFrom poweRlaw rplcon
sample_pl <- function(dat, plpars, nsamples) {
  samples <- replicate(nsamples, expr = poweRlaw::rplcon(n = nrow(dat), xmin = plpars$xmin, alpha = plpars$alpha), simplify = T)

  return(samples)
}
