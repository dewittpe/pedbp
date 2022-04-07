library(pedbp)

# error if age is out of the expected range
test <- tryCatch(height_percentile(age = -1), error = function(e) e)
stopifnot("negative age did not error" = test$message == "1 <= age & age <= 17 is not TRUE")
test <- tryCatch(height_percentile(age = 18), error = function(e) e)
stopifnot("old age did not error" = test$message == "1 <= age & age <= 17 is not TRUE")


# verify error is thrown if more than one age is passed
test <- tryCatch(height_percentile(age = c(2, 3, 8)), error = function(e) e)
stopifnot("multiple ages did not error" = test$message == "length(age) == 1L is not TRUE")

# verify a warning and rounding on non-integer age to an integer
test <- tryCatch(height_percentile(age = 3.2), warning = function(w) w)
stopifnot("rounding age down didn't work as expected" = test$message == "rounding age to: 3")

test <- tryCatch(height_percentile(age = 3.8), warning = function(w) w)
stopifnot("rounding age up didn't work as expected" = test$message == "rounding age to: 4")

# no warning when numeric, but intellectually, integer value for age is passed
test <- tryCatch(height_percentile(age = 3.00, male = 0), warning = function(w) w)
stopifnot(is.null(test))

# verify error if both male and sex are missing

test <- tryCatch(height_percentile(age = 3.00), error = function(e) e)
stopifnot(test$message == "`male` or `sex` need to specified.")
