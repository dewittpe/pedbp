library(pedbp)

################################################################################
# simple print test
print_test <-
  q_bp(p_sbp = c(0.5, 0.5), p_dbp = c(0.4, 0.32), age = 13, male = 0)

stopifnot(
  identical(
    capture.output(print_test)
  ,
    capture.output(print_test[1:2])
  )
)

################################################################################
# verify error if length(age) > 1L and length(age) != length(q_sbp) or p_sbp
# verify error if length(male) > 1L and length(male) != length(q_sbp) or p_sbp
# verify error if length(height) > 1L and length(male) != length(q_sbp) or p_sbp
tryCatch(
  q_bp(p_sbp = c(0.5, 0.5), p_dbp = c(0.4, 0.32), age = numeric(0), male = 1)
  , error = function(e) e
) |>
class() |>
identical(c("simpleError", "error", "condition")) |>
stopifnot()

tryCatch(
  q_bp(p_sbp = c(0.5, 0.5), p_dbp = c(0.4, 0.32), age = 1:3, male = 1)
  , error = function(e) e
) |>
class() |>
identical(c("simpleError", "error", "condition")) |>
stopifnot()

tryCatch(
  q_bp(p_sbp = c(0.5, 0.5), p_dbp = c(0.4, 0.32), age = 3, male = integer(0))
  , error = function(e) e
) |>
class() |>
identical(c("simpleError", "error", "condition")) |>
stopifnot()

tryCatch(
  q_bp(p_sbp = c(0.5, 0.5), p_dbp = c(0.4, 0.32), age = 3, male = 1:3)
  , error = function(e) e
) |>
class() |>
identical(c("simpleError", "error", "condition")) |>
stopifnot()

tryCatch(
  q_bp(p_sbp = c(0.5, 0.5), p_dbp = c(0.4, 0.32), age = 3, male = 1, height = numeric(0))
  , error = function(e) e
) |>
class() |>
identical(c("simpleError", "error", "condition")) |>
stopifnot()

tryCatch(
  q_bp(p_sbp = c(0.5, 0.5), p_dbp = c(0.4, 0.32), age = 3, male = 1, height = 77:79)
  , error = function(e) e
) |>
class() |>
identical(c("simpleError", "error", "condition")) |>
stopifnot()



tryCatch(
  p_bp(q_sbp = c(90,87), q_dbp = c(52, 32), age = numeric(0), male = 1)
  , error = function(e) e
) |>
class() |>
identical(c("simpleError", "error", "condition")) |>
stopifnot()

tryCatch(
  p_bp(q_sbp = c(90,87), q_dbp = c(52, 32), age = 1:3, male = 1)
  , error = function(e) e
) |>
class() |>
identical(c("simpleError", "error", "condition")) |>
stopifnot()

tryCatch(
  p_bp(q_sbp = c(90,87), q_dbp = c(52, 32), age = 3, male = integer(0))
  , error = function(e) e
) |>
class() |>
identical(c("simpleError", "error", "condition")) |>
stopifnot()

tryCatch(
  p_bp(q_sbp = c(90,87), q_dbp = c(52, 32), age = 3, male = 1:3)
  , error = function(e) e
) |>
class() |>
identical(c("simpleError", "error", "condition")) |>
stopifnot()

tryCatch(
  p_bp(q_sbp = c(90,87), q_dbp = c(52, 32), age = 3, male = 1, height = numeric(0))
  , error = function(e) e
) |>
class() |>
identical(c("simpleError", "error", "condition")) |>
stopifnot()

tryCatch(
  p_bp(q_sbp = c(90,87), q_dbp = c(52, 32), age = 3, male = 1, height = 77:79)
  , error = function(e) e
) |>
class() |>
identical(c("simpleError", "error", "condition")) |>
stopifnot()



################################################################################
# verify you get the exact row back from bp_parameters when height is ommited

d <- bp_parameters
nrow(d)
d <- d[is.na(d$height_percentile), ]
nrow(d)

for( i in 1:nrow(d)) {
  if (interactive()) {
    print(paste0("i: ", i, "; age: ", d$age[i], "; male: ", d$male[i]))
  }
  stopifnot(isTRUE(
    all.equal(
              attr(q_bp(0.5, 0.5, d$age[i], male = d$male[i]), "bp_params")
              ,
              d[i, ]
              )
    )
  )
}

for( i in 1:nrow(d)) {
  if (interactive()) {
    print(paste0("i: ", i, "; age: ", d$age[i], "; male: ", d$male[i]))
  }
  stopifnot(
    all.equal(
              attr(p_bp(90, 60, d$age[i], male = d$male[i]), "bp_params")
              ,
              d[i, ]
              )
  )
}


################################################################################
# verify expected row back for under 36 months of age with known height
d <- bp_parameters
nrow(d)
d <- d[d$age >= 12 & d$age < 36 & !is.na(d$height_percentile), ]
nrow(d)

d$ht <-
  q_length_for_age(p = d$height_percentile / 100, age = d$age, male = d$male, source = "WHO")

for( i in 1:nrow(d)) {
  if (interactive()) {
    print(paste0("i: ", i, "; age: ", d$age[i], "; male: ", d$male[i], "; height: ", d$ht[i]))
  }
  stopifnot(
    all.equal(
              attr(q_bp(0.5, 0.5, d$age[i], male = d$male[i], height = d$ht[i]), "bp_params")
              ,
              d[i, 1:8]
              )
  )
}

for( i in 1:nrow(d)) {
  if (interactive()) {
    print(paste0("i: ", i, "; age: ", d$age[i], "; male: ", d$male[i], "; height: ", d$ht[i]))
  }
  stopifnot(
    all.equal(
              attr(p_bp(90, 60, d$age[i], male = d$male[i], height = d$ht[i]), "bp_params")
              ,
              d[i, 1:8]
              )
  )
}


################################################################################
# verify expected row back for 36 months or older with known height
d <- bp_parameters
nrow(d)
d <- d[d$age >= 36 & !is.na(d$height_percentile), ]
nrow(d)

d$ht <-
  q_height_for_age( p = d$height_percentile / 100, age = d$age, male = d$male, source = "CDC")

for( i in 1:nrow(d)) {
  if (interactive()) {
    print(paste0("i: ", i, "; age: ", d$age[i], "; male: ", d$male[i], "; height: ", d$ht[i]))
  }
  stopifnot(
    all.equal(
              attr(q_bp(0.5, 0.5, d$age[i], male = d$male[i], height = d$ht[i]), "bp_params")
              ,
              d[i, 1:8]
              )
  )
}

for( i in 1:nrow(d)) {
  if (interactive()) {
    print(paste0("i: ", i, "; age: ", d$age[i], "; male: ", d$male[i], "; height: ", d$ht[i]))
  }
  stopifnot(
    all.equal(
              attr(p_bp(90, 60, d$age[i], male = d$male[i], height = d$ht[i]), "bp_params")
              ,
              d[i, 1:8]
              )
  )
}

################################################################################
##                                End of file                                 ##
################################################################################
