#' Assign mean masses to toy species
#' @param S nb species
#' @param min_size in g, linear
#' @param max_size in g, linear
#' @param assumption "log_uniform", "hutchinson", ...
#' @return vector of mean body sizes
#' @export
assign_mean_mass <- function(S, min_size, max_size, assumption = "log_uniform"){
  if(assumption == "log_uniform") {
    species_means <- exp(runif(n = S, min = log(min_size), max = log(max_size)))
  }
  if(assumption == "hutchinson") {
    species_means <- vector(length = S) 
    species_means[1] <- min_size
    for(i in 2:S) {
      species_means[i] <- 2.2 * species_means[i-1]
    }
  }
  return(species_means)
}


#' Assign individuals to species
#' @param S nb species
#' @param N nb individuals
#' @param assumption "uniform", ...
#' @return vector of species assignments
#' @export
assign_species_id <- function(S, N, assumption = "uniform"){
  species_ids <- sample.int(n = S, size = N, replace = TRUE)
  return(species_ids)
}

#' Draw a mass for an individual
#' @param species_id int species identifier
#' @param bsd vector of mean masses
#' @param assumptions list including sd
#' @return mass
#' @export 
assign_individual_mass <- function(species_id, bsd, assumptions = list(sd = .1)) {
  this_ind = rnorm(n = 1, mean = bsd[species_id], sd = (assumptions$sd * bsd[species_id]))
  return(this_ind)
}
