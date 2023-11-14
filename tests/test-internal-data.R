library(pedbp)
lms_data <- pedbp:::lms_data

################################################################################
##                 Verify the number of, and names of columns                 ##

stopifnot(identical(ncol(lms_data), 34L))

stopifnot(identical(
          names(lms_data)
          ,
          c("metric", "male", "age", "stature", "L", "M", "S", "P01", "P1",
            "P3", "P5", "P10", "P15", "P25", "P50", "P75", "P85", "P90",
            "P95", "P97", "P99", "P999", "SD4neg", "SD3neg", "SD2neg", "SD1neg",
            "SD0", "SD1", "SD2", "SD3", "SD4", "StDev", "SD5neg", "source")
          )
         )

################################################################################
##                          Verify column structure                           ##

stopifnot(inherits(lms_data[["age"]], 'numeric'))
stopifnot(inherits(lms_data[["L"]], 'numeric'), all(!is.na(lms_data[["L"]])))
stopifnot(inherits(lms_data[["M"]], 'numeric'), all(!is.na(lms_data[["M"]])))
stopifnot(inherits(lms_data[["S"]], 'numeric'), all(!is.na(lms_data[["S"]])))
stopifnot(inherits(lms_data[["stature"]], 'numeric'))
stopifnot(inherits(lms_data[["male"]], 'integer'), all(lms_data[["male"]] %in% c(0L, 1L)))
stopifnot(inherits(lms_data[["metric"]], 'character'))
stopifnot(inherits(lms_data[["source"]], 'character'))

stopifnot(identical(
  sort(unique(lms_data[["metric"]]))
  ,
  c("bmi_for_age"
    , "head_circumference_for_age"
    , "stature_for_age"
    , "weight_for_age"
    , "weight_for_stature"
   )
  )
)

stopifnot(identical(
  sort(unique(lms_data[["source"]]))
  ,
  c("CDC", "WHO")
  )
)


################################################################################
##                                End of File                                 ##
################################################################################
