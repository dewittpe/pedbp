fourth_bp_norms <-
  list(male = "./data-raw/fourth_bp_boys.dat",
       female = "./data-raw/fourth_bp_girls.dat") |>
  lapply(data.table::fread)
fourth_bp_norms <- data.table::rbindlist(fourth_bp_norms, idcol = "male")
data.table::setnames(fourth_bp_norms, old = "bp_%", new = "bp_percentile")

sbp <-
  data.table::melt(fourth_bp_norms,
                   id.vars = c("male", "age", "bp_percentile"),
                   measure.vars = 4:10,
                   variable.name = "height_percentile",
                   variable.factor = FALSE,
                   value.name = "sbp")

dbp <-
  data.table::melt(fourth_bp_norms,
                   id.vars = c("male", "age", "bp_percentile"),
                   measure.vars = 11:17,
                   variable.name = "height_percentile",
                   variable.factor = FALSE,
                   value.name = "dbp")

fourth_bp_norms <- merge(sbp, dbp)
fourth_bp_norms[, height_percentile := as.integer(height_percentile)]
fourth_bp_norms[, male := as.integer(male == "male")]

data.table::setorder(fourth_bp_norms, male, age, height_percentile, bp_percentile)
data.table::setcolorder(fourth_bp_norms, neworder = c("male", "age", "height_percentile", "bp_percentile"))
fourth_bp_norms[, age := age * 12]

fourth_bp_norms <- as.data.frame(fourth_bp_norms)
save(fourth_bp_norms, file = "./data/fourth_bp_norms.rda")

