library(pedbp)
internal_lms_data <-
  pedbp:::lms_data |>
  lapply(lapply, data.table::rbindlist, use.names = TRUE, fill = TRUE) |>
  lapply(data.table::rbindlist, use.names = TRUE, fill = TRUE) |>
  data.table::rbindlist(use.names = TRUE, fill = TRUE)

################################################################################
##                 Verify the number of, and names of columns                 ##

stopifnot(identical(ncol(internal_lms_data), 35L))

stopifnot(identical(
          names(internal_lms_data)
          ,
          c("metric", "male", "age", "L", "M", "S", "P3", "P5", "P10",
            "P25", "P50", "P75", "P85", "P90", "P95", "P97", "source", "P01",
            "P1", "P15", "P99", "P999", "SD4neg", "SD3neg", "SD2neg", "SD1neg",
            "SD0", "SD1", "SD2", "SD3", "SD4", "StDev", "SD5neg", "height",
            "length")
          )
         )

################################################################################
##                          Verify column structure                           ##

stopifnot(inherits(internal_lms_data[["age"]], 'numeric'))
stopifnot(inherits(internal_lms_data[["L"]], 'numeric'), all(!is.na(internal_lms_data[["L"]])))
stopifnot(inherits(internal_lms_data[["M"]], 'numeric'), all(!is.na(internal_lms_data[["M"]])))
stopifnot(inherits(internal_lms_data[["S"]], 'numeric'), all(!is.na(internal_lms_data[["S"]])))
stopifnot(inherits(internal_lms_data[["length"]], 'numeric'))
stopifnot(inherits(internal_lms_data[["height"]], 'numeric'))
stopifnot(inherits(internal_lms_data[["male"]], 'integer'), all(internal_lms_data[["male"]] %in% c(0L, 1L)))
stopifnot(inherits(internal_lms_data[["metric"]], 'character'))
stopifnot(inherits(internal_lms_data[["source"]], 'character'))

stopifnot(identical(
  sort(unique(internal_lms_data[["metric"]]))
  ,
  c("bmi_for_age"
    , "head_circumference_for_age"
    , "height_for_age"
    , "length_for_age"
    , "weight_for_age"
    , "weight_for_height"
    , "weight_for_length"
  )
  )
)

stopifnot(identical(
  sort(unique(internal_lms_data[["source"]]))
  ,
  c("CDC", "WHO")
  )
)


################################################################################
##                                End of File                                 ##
################################################################################
