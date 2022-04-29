cdc_length_for_age <-
  list("./data-raw/cdc_length_for_age_0_36_months.txt",
       "./data-raw/cdc_stature_for_age.txt") |>
  lapply(read.table, header = TRUE, sep = "|")

cdc_length_for_age <- do.call(rbind, cdc_length_for_age)

names(cdc_length_for_age)[1] <- c("male")
cdc_length_for_age$male <- as.integer(cdc_length_for_age$male == "male")

names(cdc_length_for_age) <- sub("X", "p", names(cdc_length_for_age))


save(cdc_length_for_age, file = "./data/cdc_length_for_age.rda")

