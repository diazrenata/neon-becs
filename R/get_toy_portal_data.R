#' Get just a little bit of Portal data
#' @return a df of Portal control plots from 1994-1995
#' @export
get_toy_portal_data <- function() {
  portal_data <- portalr::summarise_individual_rodents(clean = T, type = "Granivores", unknowns = F, time = "date") %>%
    dplyr::filter(year %in% c(1994, 1995), treatment == "control", !is.na(wgt)) %>%
    dplyr::select(species, wgt) %>%
    dplyr::rename(individual_sizes = wgt, individual_species_ids = species)

  return(portal_data)
}
