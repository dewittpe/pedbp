################################################################################
# gemelli1990.R

gemelli1990 <-
  lapply(list(
             f = "./data-raw/gemelli1990_female.csv",
             m = "./data-raw/gemelli1990_male.csv"
             ),
         read.table
             , header = TRUE
             , sep = ","
             , strip.white = TRUE
  )
gemelli1990 <- data.table::rbindlist(gemelli1990, idcol = "male")

gemelli1990[, X := NULL]
gemelli1990[, n := NULL]
gemelli1990[, HR_sd := NULL]
gemelli1990[, HR_mean := NULL]
gemelli1990[, wt_gr_sd := NULL]
gemelli1990[, wt_gr_mean := NULL]
data.table::setnames(gemelli1990, old = "age_mo", "age")
data.table::setnames(gemelli1990, old = "SBP_mean", "sbp_mean")
data.table::setnames(gemelli1990, old = "SBP_sd", "sbp_sd")
data.table::setnames(gemelli1990, old = "DBP_mean", "dbp_mean")
data.table::setnames(gemelli1990, old = "DBP_sd", "dbp_sd")


# rename and set units
gemelli1990$male = as.integer(gemelli1990$male == "m")

gemelli1990 <- as.data.frame(gemelli1990)

save(gemelli1990, file = "./data/gemelli1990.rda")

################################################################################
#                                 End of File                                  #
################################################################################

