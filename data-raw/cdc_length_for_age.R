cdc_length_for_age <-
  list("./data-raw/cdc_length_for_age_0_36_months.txt",
       "./data-raw/cdc_stature_for_age.txt") |>
  lapply(read.table, header = TRUE, sep = "|")

cdc_length_for_age <- do.call(rbind, cdc_length_for_age)

names(cdc_length_for_age)[1] <- c("male")
cdc_length_for_age$male <- as.integer(cdc_length_for_age$male == "male")

data.table::setDT(cdc_length_for_age)

cdc_length_for_age <-
  data.table::melt(cdc_length_for_age,
                   id.vars= c("male", "age"),
                   variable.name = "height_percentile",
                   variable.factor = FALSE,
                   value.name = "height")

cdc_length_for_age[, height_percentile := as.integer(sub("X", "", height_percentile))]

data.table::setorder(cdc_length_for_age, male, age, height_percentile)
cdc_length_for_age

cdc_length_for_age <- as.data.frame(cdc_length_for_age)
save(cdc_length_for_age, file = "./data/cdc_length_for_age.rda")

