#
# # getTaxonTable gives species info
# species_table <- neonUtilities::getTaxonTable(taxonType = "SMALL_MAMMAL")
# str(species_table)

download_neon_data <- function(){

neonUtilities::zipsByProduct(dpID = "DP1.10072.001", site = "all",
                             savepath = here::here("inst", "data", "raw"))

  # downloads 737 files totaling 48.3 MB
}
