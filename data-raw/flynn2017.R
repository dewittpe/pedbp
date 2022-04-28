################################################################################
# Data on blood pressure and height form Flynn et.al. (2017).
#
# Male   data is transcribed from Table 4 in Flynn et.al.(2017)
# Female data is transcribed from Table 5 in Flynn et.al.(2017)
#
################################################################################
# build a data.frame from the raw data

# scan the data
bp_male   <- scan(file = "./data-raw/flynn2017_male.txt",   what = character(), sep = "\n")
bp_female <- scan(file = "./data-raw/flynn2017_female.txt", what = character(), sep = "\n")

# build a data.frame
bp <-
  data.frame(
               male = c(rep(1L, length(bp_male)), rep(0L, length(bp_female)))
             , age_years = rep(rep(seq(1, 17, by = 1), each = 6), 2)
             , bp_percentile = rep(c("height_in", "height_cm", "50th", "90th", "95th", "95th + 12 mmHg"), times = 2 * 17)
             )

# split the raw values into multiple columns
mmHg <-
  sub("^50th\\s", "",
    sub("^90th\\s", "",
      sub("^95th\\s", "",
        sub("^95th\\s\\+\\s12\\smm\\sHg\\s", "",
          sub("^\\d*\\s*Height\\s\\(.+\\)\\s", "", c(bp_male, bp_female))
          )
        )
      )
    )

mmHg <- do.call(rbind, strsplit(mmHg, " "))
colnames(mmHg) <-
  c(paste0("sbp_height_percentile_", c(5, 10, 25, 50, 75, 90, 95)),
    paste0("dbp_height_percentile_", c(5, 10, 25, 50, 75, 90, 95)))

bp <- cbind(bp, mmHg)
data.table::setDT(bp)

# extract the heights and build a data.frame for just these percentiles and
# measurements.
heights <- subset(bp, grepl("^height_", bp_percentile))
head(heights)

heights <-
  data.table::melt(heights,
                   id.vars = c("age_years", "male", "bp_percentile"),
                   variable.factor = FALSE,
                   variable.name = "height_percentile")
heights <- subset(heights, grepl("^sbp_height_percentile_", height_percentile))

heights[, height_percentile := as.numeric(sub("^.+_(\\d+)$", "\\1", height_percentile))]
heights[, value := as.numeric(value)]

heights <- data.table::dcast(heights, age_years + male + height_percentile ~ bp_percentile)
heights

# extract the BP measurements and get them into a useful format
head(bp)
bp <- subset(bp, !grepl("^height_", bp_percentile))
#bp <- subset(bp, !grepl("12 mmHg", bp_percentile))
bp <- data.table::melt(bp,
                       variable.factor = FALSE,
                       id.vars = c("age_years", "male", "bp_percentile"),
                       value.name = "mmHg")

bp[, mmHg := as.numeric(mmHg)]
bp[, height_percentile := as.numeric(sub("^.+_(\\d+)$", "\\1", variable))]
bp[, variable := sub("^((s|d)bp)_.+$", "\\1", variable)]
#bp[, bp_percentile := as.numeric(sub("^(\\d+).+", "\\1", bp_percentile))]

bp <-
  data.table::dcast(bp
    , age_years + male + height_percentile + bp_percentile ~ variable
    , value.var = "mmHg")


# merge into one data set
bp <- merge(bp, heights, all = TRUE)

# unify names and units
bp[, age_years := age_years * 12]
data.table::setnames(bp, old = "age_years", new = "age")
data.table::setnames(bp, old = "height_cm", new = "height")
bp[, height_in := NULL]

# save the data set as a simple data.frame
flynn2017 <- as.data.frame(bp)
save(flynn2017, file = "./data/flynn2017.rda")

################################################################################
#                                 End of File                                  #
################################################################################

