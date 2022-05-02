nhlbi_bp_norms <-
  list(male = "./data-raw/nhlbi_bp_norms_boys.csv",
       female = "./data-raw/nhlbi_bp_norms_girls.csv") |>
  lapply(data.table::fread)
nhlbi_bp_norms <- data.table::rbindlist(nhlbi_bp_norms, idcol = "male")
data.table::setnames(nhlbi_bp_norms, old = "bp_%", new = "bp_percentile")

sbp <-
  data.table::melt(nhlbi_bp_norms,
                   id.vars = c("male", "age", "bp_percentile"),
                   measure.vars = 4:10,
                   variable.name = "height_percentile",
                   variable.factor = FALSE,
                   value.name = "sbp")

dbp <-
  data.table::melt(nhlbi_bp_norms,
                   id.vars = c("male", "age", "bp_percentile"),
                   measure.vars = 11:17,
                   variable.name = "height_percentile",
                   variable.factor = FALSE,
                   value.name = "dbp")

nhlbi_bp_norms <- merge(sbp, dbp)
nhlbi_bp_norms[, height_percentile := as.integer(height_percentile)]
nhlbi_bp_norms[, male := as.integer(male == "male")]

data.table::setorder(nhlbi_bp_norms, male, age, height_percentile, bp_percentile)
data.table::setcolorder(nhlbi_bp_norms, neworder = c("male", "age", "height_percentile", "bp_percentile"))
nhlbi_bp_norms[, age := age * 12]

nhlbi_bp_norms <- as.data.frame(nhlbi_bp_norms)
save(nhlbi_bp_norms, file = "./data/nhlbi_bp_norms.rda")

