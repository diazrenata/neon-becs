#' Download NEON data
#'
#' Download raw NEON small mammal data using neonUtilities.
#'
#' @export
#' @importFrom neonUtilities zipsByProduct stackByTable
#' @importFrom here here
download_neon_data <- function(){

  neonUtilities::zipsByProduct(dpID = "DP1.10072.001", site = "all",
                               savepath = here::here("inst", "data", "raw"))

  # downloads 737 files totaling 48.3 MB
  filepath_forstack <- file.path("inst", "data", "raw", "filesToStack10072")

  neonUtilities::stackByTable(filepath = filepath_forstack, folder = T)
}

#' Process NEON data
#' Load NEON mammal trapping data
#' Filter to target taxa and remove problem sites following Read et al 2018 Ecography: https://figshare.com/articles/A_thermal_gradient_of_trait_similarity_across_North_America/5339407
#' @return TRUE if successful
#' @export
#' @importFrom here here
#' @importFrom neonUtilities getTaxonTable
#' @importFrom dplyr select filter distinct
process_neon_data <- function(){

  variables <- read.csv(here::here("inst", "data", "raw", "filesToStack10072", "stackedFiles", "variables.csv"), stringsAsFactors = F)

  mam_perplotnight <- read.csv(here::here("inst", "data", "raw", "filesToStack10072", "stackedFiles", "mam_perplotnight.csv"), stringsAsFactors = F)

  mam_pertrapnight <- read.csv(here::here("inst", "data", "raw", "filesToStack10072", "stackedFiles", "mam_pertrapnight.csv"), stringsAsFactors = F)

  species_table <- neonUtilities::getTaxonTable(taxonType = "SMALL_MAMMAL")

  # Target taxa from supplement to Read et al 2018 Ecography
  if(!dir.exists(here::here("inst", "data", "raw", "read_ecography"))) {
    dir.create(here::here("inst", "data", "raw", "read_ecography"), recursive = TRUE)
  }
  download.file(url = "https://ndownloader.figshare.com/files/9167545",
                destfile = here::here("inst", "data", "raw", "read_ecography", "raw_NEON_mammal_data.csv"))
  download.file(url = "https://ndownloader.figshare.com/files/9821962",
                destfile = here::here("inst", "data", "raw", "read_ecography", "code_supplement.R"))
  target_taxa <- read.csv(here::here("inst", "data", "raw", "read_ecography", "raw_NEON_mammal_data.csv"), stringsAsFactors = F)

  target_taxa <- target_taxa %>%
    dplyr::select(taxonID, taxonProtocolCategory) %>%
    dplyr::filter(taxonProtocolCategory == "target") %>%
    dplyr::select(taxonID) %>%
    dplyr::distinct()

  # Sites to exclude from code supplement to Read et al 2018 Ecography
  # https://figshare.com/articles/A_thermal_gradient_of_trait_similarity_across_North_America/5339407

  rodents <- species_table %>%
    dplyr::filter(order == "Rodentia",
                  taxonRank == "species",
                  taxonID %in% target_taxa$taxonID)

  captures <- mam_pertrapnight %>%
    dplyr::filter(trapStatus == "5 - capture", fate != "nontarget",
                  !is.na(weight), taxonID %in% c(rodents$taxonID, rodents$acceptedTaxonID), !(siteID %in% c('DSNY', 'DELA', 'HEAL'))) %>%
    dplyr::select(siteID, taxonID, weight)

  unique_sites <- unique(captures$siteID)

  save_single_site <- function(this_siteID, captures) {
    this_site <- dplyr::filter(captures, siteID == this_siteID) %>%
      dplyr::select(taxonID, weight)

    write.csv(this_site, here::here("inst", "data", "processed", paste0(this_siteID, "_processed.csv")), row.names = F)
  }

  lapply(unique_sites, FUN = save_single_site, captures = captures)

  return(TRUE)
}

#' Load NEON data
#'
#' @return list of NEON community data
#' @export
#' @importFrom here here
load_neon_data <- function() {
  neon_paths <- list.files(path = here::here("inst", "data", "processed"), full.names = TRUE)
  neon_filenames <- list.files(path = here::here("inst", "data", "processed"))

  trim_neon_names <- function(neon_site_filename) {
    this_name <- strsplit(neon_site_filename, split = "_processed.csv")[[1]]
    return(this_name)
  }
  neon_names <- vapply(neon_filenames, FUN = trim_neon_names, FUN.VALUE = "ABBY")

  neon_data <- lapply(neon_paths, FUN = read.csv, stringsAsFactors = F)

  names(neon_data) <- neon_names

  return(neon_data)
}
