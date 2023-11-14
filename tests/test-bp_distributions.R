library(pedbp)


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
  q_stature_for_age(p = d$height_percentile / 100, age = d$age, male = d$male)

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
  q_stature_for_age( p = d$height_percentile / 100, age = d$age, male = d$male)

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
