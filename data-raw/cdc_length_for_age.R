cdc_length_for_age <-
  list("./data-raw/cdc_length_for_age_0_36_months.txt",
       "./data-raw/cdc_stature_for_age.txt") |>
  lapply(read.table, header = TRUE, sep = "|")

cdc_length_for_age <- do.call(rbind, cdc_length_for_age)

names(cdc_length_for_age)[1] <- c("male")
cdc_length_for_age$male <- as.integer(cdc_length_for_age$male == "male")

names(cdc_length_for_age) <- sub("X", "p", names(cdc_length_for_age))


# pull in the code for estimating the distribution means and sd
set.seed(42)
source("R/est_norm.R")
ps <- as.numeric(sub("p", "", names(cdc_length_for_age)[-(1:2)])) / 100

ms <- apply(cdc_length_for_age[, -(1:2)], 1, est_norm, ps)
ms <- lapply(ms, getElement, "par")
ms <- do.call(rbind, ms)
colnames(ms) <- c("mean_height", "sd_height")
cdc_length_for_age <- cbind(cdc_length_for_age, ms)

save(cdc_length_for_age, file = "./data/cdc_length_for_age.rda")

