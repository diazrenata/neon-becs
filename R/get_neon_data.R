
download_neon_data <- function(){

  neonUtilities::zipsByProduct(dpID = "DP1.10072.001", site = "all",
                               savepath = here::here("inst", "data", "raw"))

  # downloads 737 files totaling 48.3 MB
  filepath_forstack <- file.path("inst", "data", "raw", "filesToStack10072")

  neonUtilities::stackByTable(filepath = filepath_forstack, folder = T)
}

load_neon_data <- function(){

  variables <- read.csv(here::here("inst", "data", "raw", "filesToStack10072", "stackedFiles", "variables.csv"), stringsAsFactors = F)

  mam_perplotnight <- read.csv(here::here("inst", "data", "raw", "filesToStack10072", "stackedFiles", "mam_perplotnight.csv"), stringsAsFactors = F)

  mam_pertrapnight <- read.csv(here::here("inst", "data", "raw", "filesToStack10072", "stackedFiles", "mam_pertrapnight.csv"), stringsAsFactors = F)

  species_table <- neonUtilities::getTaxonTable(taxonType = "SMALL_MAMMAL")

  captures <- mam_pertrapnight %>%
    dplyr::filter(trapStatus == "5 - capture", fate != "nontarget",
                  !is.na(weight))

  }
