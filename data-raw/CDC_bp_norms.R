cdc_bp_norms <-
  list(male = "./data-raw/CDC_bp_norms_boys.csv",
       female = "./data-raw/CDC_bp_norms_girls.csv") |>
  lapply(data.table::fread)
cdc_bp_norms <- data.table::rbindlist(cdc_bp_norms, idcol = "male")
data.table::setnames(cdc_bp_norms, old = "bp_%", new = "bp_percentile")

sbp <-
  data.table::melt(cdc_bp_norms,
                   id.vars = c("male", "age", "bp_percentile"),
                   measure.vars = 4:10,
                   variable.name = "height_percentile",
                   variable.factor = FALSE,
                   value.name = "sbp")

dbp <-
  data.table::melt(cdc_bp_norms,
                   id.vars = c("male", "age", "bp_percentile"),
                   measure.vars = 11:17,
                   variable.name = "height_percentile",
                   variable.factor = FALSE,
                   value.name = "dbp")

cdc_bp_norms <- merge(sbp, dbp)
cdc_bp_norms[, height_percentile := as.integer(height_percentile)]
cdc_bp_norms[, male := as.integer(male == "male")]

data.table::setorder(cdc_bp_norms, male, age, height_percentile, bp_percentile)
data.table::setcolorder(cdc_bp_norms, neworder = c("male", "age", "height_percentile", "bp_percentile"))
cdc_bp_norms[, age := age * 12]

cdc_bp_norms <- as.data.frame(cdc_bp_norms)
save(cdc_bp_norms, file = "./data/cdc_bp_norms.rda")

