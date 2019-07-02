#' @title Make ISD
#' @param community_energy community df with energy & sizeclass columns added
#' @return ISD table
#' @export
make_isd <- function(community_energy){
  this_isd <- community_energy %>%
    dplyr::mutate(ln_size = log(individual_sizes), ln_energy = log(individual_energy))

  return(this_isd)
}

#' Fit GMM to individual size distribution
#' Using `mclust::Mclust`
#' @param isd result of `make_isd`
#' @return mclust fit
#' @export
#' @importFrom mclust Mclust mclustBIC mclust.options emControl densityMclust
#' @importFrom dplyr filter
fit_gmm <- function(isd){
  isd <- dplyr::filter(isd, !is.na(ln_size))
  this_fit <- mclust::densityMclust(isd$ln_size, G = 1:15, modelNames = "V",
                 prior = NULL,
                 control = emControl(),
                 initialization = NULL,
                 warn = mclust.options("warn"),
                 x =  NULL,
                 verbose = FALSE)
  return(this_fit)
}

#' Get number of gaussians
#' @param gmm result of fit_gmm
#' @return ngaussians
#' @export
get_ngaussians <- function(gmm) {
  return(gmm$G)
}


#' Get modes of PDF
#' @param isd_pdf result of get_pdf
#' @importFrom pastecs turnpoints
#' @importFrom dplyr filter
#' @importFrom graphics plot
#' @export
get_modes <- function(isd_pdf) {
  mode_ps <- pastecs::turnpoints(isd_pdf$density)
  modes <- isd_pdf[which(mode_ps$peaks), ]
  modes <- modes$sizes
  return(modes)
}

#' Get PDF from fitted gmm
#' @param gmm result of fit_gmm
#' @return pdf vector
#' @importFrom stats predict
#' @export
get_pdf <- function(gmm) {
  sizes <- seq(0, 8, by = 0.01)
  gmm_pdf <- predict(gmm, newdata = sizes, what = "dens", logarithm = F)
  pdf <- data.frame(sizes = sizes, density = gmm_pdf)
  return(pdf)
}

#' Get means of gaussians
#' @param gmm result of fit_gmm
#' @return means of gaussians
#' @export
get_gaussianmeans <- function(gmm){
  return(gmm$parameters$mean)
}

#' Get BIC for modes
#' @param gmm result of fit_gmm
#' @return BIC of modes
#' @export
get_bic <- function(gmm){
  return(gmm$BIC)
}
