library(pedbp)

# expect error if height_unit has a length != 1L
test <-
  tryCatch(
    bp_chart(age = 13, height = 132, height_unit = NULL)
    , error = function(e) e
  )

stopifnot("height_units are null did not error" = inherits(test, "error"))
