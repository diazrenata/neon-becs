library(neonbecs)

dat <- get_toy_portal_data()

head(dat)

dat_energy <- dat %>%
  replicatebecs::add_energy_sizeclass()

dat_isd <- dat_energy %>%
  make_isd()

library(VGAM)

lower = 4.8
upper =119

fit3 <- vglm(individual_sizes ~ 1, paretoff(), data = dat, trace = TRUE)

coef(fit3, matrix = TRUE)
c(fit3@misc$lower, fit3@misc$upper)

library(poweRlaw)
m = conpl$new(dat$individual_sizes)
estimate_xmin(m)
m$setXmin(46)
estimate_pars(m)


j = rplcon(n = 100, xmin = 46, alpha = 10.16)
plot(j)
