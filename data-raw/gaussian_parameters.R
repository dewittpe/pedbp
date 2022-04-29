# Build a data set for the mean and standard deviations for Gaussian
# approximations of the percentiles

# pull in the code for estimating the distribution means and sd
set.seed(42)
source("R/est_norm.R")
load("./data/cdc_length_for_age.rda")
load("./data/gemelli1990.rda")
load("./data/lo2013.rda")
load("./data/cdc_bp_norms.rda")

data.table::setDT(cdc_length_for_age)
data.table::setDT(cdc_bp_norms)
data.table::setDT(gemelli1990)
data.table::setDT(lo2013)

# mean and standard deviation estimates for height
ht_parameters <-
  cdc_length_for_age[,
                     as.list(est_norm(height, height_percentile/100)$par)
                     , by = .(male, age)
                     ]

data.table::setnames(ht_parameters, old = c("mean", "sd"), new = c("height_mean", "height_sd"))

# bp
sbp <- cdc_bp_norms[, as.list(est_norm(sbp, bp_percentile/100)$par), by = .(male, age, height_percentile)]
dbp <- cdc_bp_norms[, as.list(est_norm(dbp, bp_percentile/100)$par), by = .(male, age, height_percentile)]
data.table::setnames(sbp, old = c("mean", "sd"), new = c("sbp_mean", "sbp_sd"))
data.table::setnames(dbp, old = c("mean", "sd"), new = c("dbp_mean", "dbp_sd"))
cdc_bp <- merge(sbp, dbp)

bp_parameters <- rbind(gemelli1990, cdc_bp, lo2013, use.names = TRUE, fill = TRUE)


bp_parameters <- as.data.frame(bp_parameters)
ht_parameters <- as.data.frame(ht_parameters)

save(ht_parameters, file = "./data/ht_parameters.rda") # save first (Makefile logic)
save(bp_parameters, file = "./data/bp_parameters.rda") # save last  (Makefile logic)
