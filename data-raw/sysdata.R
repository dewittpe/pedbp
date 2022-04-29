# create a LMS data set for internal use in the package

bmi_for_age           <- data.table::fread("./data-raw/cdc_percentile_data_with_lms_values/bmiagerev.csv")#[, .(Sex, Agemos, L, M, S)]
head_circ_for_age     <- data.table::fread("./data-raw/cdc_percentile_data_with_lms_values/hcageinf.csv") #[, .(Sex, Agemos, L, M, S)]
length_for_age_inf    <- data.table::fread("./data-raw/cdc_percentile_data_with_lms_values/lenageinf.csv")#[, .(Sex, Agemos, L, M, S)]
stature_for_age       <- data.table::fread("./data-raw/cdc_percentile_data_with_lms_values/statage.csv")  #[, .(Sex, Agemos, L, M, S)]
weight_for_age        <- data.table::fread("./data-raw/cdc_percentile_data_with_lms_values/wtage.csv")    #[, .(Sex, Agemos, L, M, S)]
weight_for_age_inf    <- data.table::fread("./data-raw/cdc_percentile_data_with_lms_values/wtageinf.csv") #[, .(Sex, Agemos, L, M, S)]
weight_for_length_inf <- data.table::fread("./data-raw/cdc_percentile_data_with_lms_values/wtleninf.csv") #[, .(Sex, Length, L, M, S)]
weight_for_stature    <- data.table::fread("./data-raw/cdc_percentile_data_with_lms_values/wtstat.csv")   #[, .(Sex, Height, L, M, S)]

cdc_lms_data <-
  data.table::rbindlist(
    list(
           bmi_for_age           = bmi_for_age
         , head_circ_for_age     = head_circ_for_age
         , length_for_age_inf    = length_for_age_inf
         , stature_for_age       = stature_for_age
         , weight_for_age        = weight_for_age
         , weight_for_age_inf    = weight_for_age_inf
         , weight_for_length_inf = weight_for_length_inf
         , weight_for_stature    = weight_for_stature
    )
    , idcol = "set"
    , use.names = TRUE
    , fill = TRUE
  )

names(cdc_lms_data)[2] <- "male"
names(cdc_lms_data)[3] <- "age"
names(cdc_lms_data) <- tolower(names(cdc_lms_data))
cdc_lms_data$male <- as.integer(cdc_lms_data$male == "1")

cdc_lms_data <-
  cdc_lms_data[, .(set, male, age, length, height, l, m, s,
                   p03=p3, p05=p5, p10, p25, p50, p75, p85, p90, p95, p97)]


cdc_lms_data <- as.data.frame(cdc_lms_data)
usethis::use_data(cdc_lms_data, internal = TRUE, overwrite = TRUE)

