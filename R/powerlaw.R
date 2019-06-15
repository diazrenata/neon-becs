#' Fit powerlaw to community ISD
#'
#' @param dat community df with `individual_sizes` column
#' @param estimate_min whether to estimate the minimum for the power law, or use the minimum of `individual_sizes`. Defaults TRUE for estimate.
#' @return dataframe of simulated dat drawn from fitted power law
#' @export
fit_power_law <- function(dat, estimate_min = TRUE) {

  library(poweRlaw)

  m = conpl$new(dat$individual_sizes)
  if(estimate_min) {
    minimum_x = estimate_xmin(m)$xmin
  } else {
    minimum_x = min(dat$individual_sizes)
  }
  m$setXmin(minimum_x)
  estimated_alpha = estimate_pars(m)$pars

  sizevect = rplcon(n = nrow(dat), xmin = minimum_x, alpha = estimated_alpha)

  pl_dat <- data.frame(individual_species_ids = NA,
                       individual_sizes = sizevect)
  rm(m)
  return(pl_dat)
}
