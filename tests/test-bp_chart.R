library(pedbp)

################################################################################
# expect error if height_unit has a length != 1L
test <-
  tryCatch(
    bp_chart(age = 13, male = 1, height = 132, height_unit = NULL)
    , error = function(e) e
  )

stopifnot(!is.null(test) &&
  grepl("length(height_unit) == 1L is not TRUE", test$message, fixed = TRUE)
)

################################################################################
#
