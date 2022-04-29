################################################################################
# lo2013.R
#
# Import the data from Lo et.al. (2013) for blood pressure, weight, height, and
# bmi means and standard deviations.
#

lo2013 <-
  read.table(
             file = "./data-raw/lo2013_bp_weight_height_bmi.txt"
             , header = TRUE
             , sep = "|"
             , strip.white = TRUE
  )


# rename and set units
lo2013$male = as.integer(lo2013$sex == "male")
lo2013$age <- lo2013$age * 12

names(lo2013) <- sub("_mmHg", "", names(lo2013))
names(lo2013) <- sub("_cm", "", names(lo2013))
names(lo2013) <- sub("_kg", "", names(lo2013))

head(lo2013)

lo2013 <- lo2013[c("age", "male", "mean_sbp", "sd_sbp", "mean_dbp", "sd_dbp")]
names(lo2013) <- c("age", "male", "sbp_mean", "sbp_sd", "dbp_mean", "dbp_sd")

save(lo2013, file = "./data/lo2013.rda")

################################################################################
#                                 End of File                                  #
################################################################################

