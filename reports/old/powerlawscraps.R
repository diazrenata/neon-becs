library(neonbecs)

dat <- get_toy_portal_data()

head(dat)

dat_energy <- dat %>%
  replicatebecs::add_energy_sizeclass()

dat_isd <- dat_energy %>%
  make_isd()



powerlaw_comm <- data.frame(individual_species_ids = NA,
                            individual_sizes = j) %>%
  replicatebecs::add_energy_sizeclass() %>%
  make_isd()

plot_isd(dat_isd)
plot_isd(powerlaw_comm)



sizevect = rplcon(n = nrow(dat), xmin = minimum_x, alpha = estimated_alpha)

pl_dat <- data.frame(individual_species_ids = NA,
                     individual_sizes = sizevect)
dplcon(x, xmin, alpha, log = FALSE)
