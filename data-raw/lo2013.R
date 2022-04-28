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

save(lo2013, file = "./data/lo2013.rda")

################################################################################
#                                 End of File                                  #
################################################################################

