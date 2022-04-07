library(pedbp)

# error if age is out of the expected range
test <- tryCatch(height_percentile(age = -1), error = function(e) e)
stopifnot("negative age did not error" = test$message == "1 <= age & age <= 17 is not TRUE")
test <- tryCatch(height_percentile(age = 18), error = function(e) e)
stopifnot("old age did not error" = test$message == "1 <= age & age <= 17 is not TRUE")


# verify error is thrown if more than one age is passed
test <- tryCatch(height_percentile(age = c(2, 3, 8)), error = function(e) e)
stopifnot("multiple ages did not error" = test$message == "length(age) == 1L is not TRUE")
