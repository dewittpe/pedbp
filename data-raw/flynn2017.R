flynn2017 <-
  list(male = "./data-raw/flynn2017_bp_boys.dat",
       female = "./data-raw/flynn2017_bp_girls.dat") |>
  lapply(data.table::fread)
flynn2017 <- data.table::rbindlist(flynn2017, idcol = "male")
data.table::setnames(flynn2017, old = "bp_%", new = "bp_percentile")

sbp <-
  data.table::melt(flynn2017,
                   id.vars = c("male", "age", "bp_percentile"),
                   measure.vars = 4:10,
                   variable.name = "height_percentile",
                   variable.factor = FALSE,
                   value.name = "sbp")

dbp <-
  data.table::melt(flynn2017,
                   id.vars = c("male", "age", "bp_percentile"),
                   measure.vars = 11:17,
                   variable.name = "height_percentile",
                   variable.factor = FALSE,
                   value.name = "dbp")

flynn2017 <- merge(sbp, dbp)
flynn2017[, height_percentile := as.integer(height_percentile)]
flynn2017[, male := as.integer(male == "male")]

data.table::setorder(flynn2017, male, age, height_percentile, bp_percentile)
data.table::setcolorder(flynn2017, neworder = c("male", "age", "height_percentile", "bp_percentile"))
flynn2017[, age := age * 12]

flynn2017 <- as.data.frame(flynn2017)
save(flynn2017, file = "./data/flynn2017.rda")

