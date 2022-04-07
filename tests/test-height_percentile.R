library(pedbp)

# error if age is out of the expected range
test <- tryCatch(height_percentile(age = -1), error = function(e) e)
stopifnot("negative age did not error" = !is.null(test) && test$message == "1 <= age & age <= 17 is not TRUE")
test <- tryCatch(height_percentile(age = 18), error = function(e) e)
stopifnot("old age did not error" = !is.null(test) && test$message == "1 <= age & age <= 17 is not TRUE")


# verify error is thrown if more than one age is passed
test <- tryCatch(height_percentile(age = c(2, 3, 8)), error = function(e) e)
stopifnot("multiple ages did not error" = !is.null(test) && test$message == "length(age) == 1L is not TRUE")

# verify a warning and rounding on non-integer age to an integer
test <- tryCatch(height_percentile(age = 3.2), warning = function(w) w)
stopifnot("rounding age down didn't work as expected" = !is.null(test) && test$message == "rounding age to: 3")

test <- tryCatch(height_percentile(age = 3.8), warning = function(w) w)
stopifnot("rounding age up didn't work as expected" = !is.null(test) && test$message == "rounding age to: 4")

# no warning when numeric, but intellectually, integer value for age is passed
test <- tryCatch(height_percentile(height = 81.0, height_unit = "cm", age = 3.00, male = 0), warning = function(w) w)
stopifnot(is.null(test))

# verify error if both male and sex are missing
test <- tryCatch(height_percentile(age = 3.00), error = function(e) e)
stopifnot(!is.null(test) && test$message == "`male` or `sex` need to specified.")

# verify error if both male and sex are specified.
test <- tryCatch(height_percentile(age = 3.00, male = 1, sex = "M"), error = function(e) e)
stopifnot(!is.null(test) && test$message == "only one of `male` or `sex` should be specified.")

# error if sex is not, case invariant, in c("m", "male", "f", "female")
test <- tryCatch(height_percentile(age = 3, sex = "x"), error = function(e) e)
stopifnot(!is.null(test) && grepl('tolower(sex) %in% c("m", "male", "f", "female") is not TRUE', test$message, fixed = TRUE))

# error if male is not 0 or 1
test <- tryCatch(height_percentile(age = 3, male = 2), error = function(e) e)
stopifnot(!is.null(test) && grepl('male %in% 0:1 is not TRUE', test$message, fixed = TRUE))

# error if sex or male has length greater than one
test <- tryCatch(height_percentile(age = 3, sex = c("m", "male", "x")), error = function(e) e)
stopifnot(!is.null(test) && grepl('length(sex) == 1L is not TRUE', test$message, fixed = TRUE))

test <- tryCatch(height_percentile(age = 3, male = 0:1), error = function(e) e)
stopifnot(!is.null(test) && grepl('length(male) == 1L is not TRUE', test$message, fixed = TRUE))

# error if height_unit is longer than 1L
test <- tryCatch(height_percentile(height_unit = c("in", "cm"), age = 3, male = 0), error = function(e) e)
stopifnot(!is.null(test) && grepl('length(height_unit) == 1L is not TRUE', test$message, fixed = TRUE))

# error if height_unit is not an expected unit
test <- tryCatch(height_percentile(height_unit = c("inch"), age = 3, male = 0), error = function(e) e)
stopifnot(!is.null(test) && grepl('height_unit %in% c("in", "inches", "cm", "centimeters") is not TRUE' , test$message, fixed = TRUE))

# error if height is less than or equal to  zero
test <- tryCatch(height_percentile(height = -1.2, height_unit = c("inches"), age = 3, male = 0), error = function(e) e)
stopifnot(!is.null(test) && grepl('height > 0 is not TRUE' , test$message, fixed = TRUE))

# error if height has length greater than 1
test <- tryCatch(height_percentile(height = c(1, 2), height_unit = c("inches"), age = 3, male = 0), error = function(e) e)
stopifnot(!is.null(test) && grepl('length(height) == 1L is not TRUE' , test$message, fixed = TRUE))
