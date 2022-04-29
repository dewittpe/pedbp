# Build a data set for the mean and standard deviations for Gaussian
# approximations of the percentiles

# pull in the code for estimating the distribution means and sd
set.seed(42)
source("R/est_norm.R")
load("./data/cdc_length_for_age.rda")
load("./data/gemelli1990.rda")
load("./data/flynn2017.rda")

ps <- names(cdc_length_for_age)
ps <- ps[grepl("^p", ps)]
ps <- as.numeric(sub("p", "", ps)) / 100

ms <- apply(cdc_length_for_age[, -(1:2)], 1, est_norm, ps)
ms <- lapply(ms, getElement, "par")
ms <- do.call(rbind, ms)
colnames(ms) <- c("mean_height", "sd_height")

gaussian_parameters <- cbind(cdc_length_for_age[, c("age", "male")], ms)

head(gaussian_parameters)


subset(cdc_length_for_age, male == 0 & age > 12 & age < 13)
gemelli1990
head(flynn2017)
