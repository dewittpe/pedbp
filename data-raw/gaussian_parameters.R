# Build a data set for the mean and standard deviations for Gaussian
# approximations of the percentiles

# pull in the code for estimating the distribution means and sd
set.seed(42)
source("R/est_norm.R")
load("./data/gemelli1990.rda")
load("./data/lo2013.rda")
load("./data/nhlbi_bp_norms.rda")
load("./data/fourth_bp_norms.rda")

data.table::setDT(nhlbi_bp_norms)
data.table::setDT(gemelli1990)
data.table::setDT(lo2013)
data.table::setDT(fourth_bp_norms)

sbp <- nhlbi_bp_norms[, as.list(est_norm(sbp, bp_percentile/100)$par), by = .(male, age, height_percentile)]
dbp <- nhlbi_bp_norms[, as.list(est_norm(dbp, bp_percentile/100)$par), by = .(male, age, height_percentile)]
data.table::setnames(sbp, old = c("mean", "sd"), new = c("sbp_mean", "sbp_sd"))
data.table::setnames(dbp, old = c("mean", "sd"), new = c("dbp_mean", "dbp_sd"))
cdc_bp <- merge(sbp, dbp)

sbp <- fourth_bp_norms[, as.list(est_norm(sbp, bp_percentile/100)$par), by = .(male, age, height_percentile)]
dbp <- fourth_bp_norms[, as.list(est_norm(dbp, bp_percentile/100)$par), by = .(male, age, height_percentile)]
data.table::setnames(sbp, old = c("mean", "sd"), new = c("sbp_mean", "sbp_sd"))
data.table::setnames(dbp, old = c("mean", "sd"), new = c("dbp_mean", "dbp_sd"))
fourth_bp <- merge(sbp, dbp)

bp_parameters <-
  data.table::rbindlist(list(gemelli1990 = gemelli1990, nhlbi = cdc_bp, lo2013 = lo2013, fourth = fourth_bp), idcol = "source", use.names = TRUE, fill = TRUE)

bp_parameters <- as.data.frame(bp_parameters)

save(bp_parameters, file = "./data/bp_parameters.rda")
