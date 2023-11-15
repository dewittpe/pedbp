#' LMS Look Up Functions
#'
#' Non-exported functions to look up LMS values from the internal data set
#' lms_data
#'
#' @param metric section of the look up tabl
#' @param male   0 = female; 1 = male
#' @param source See Details in \code{\link{pediatric_growth_standards}}
#' @param age    in months
#' @param stature height/length in centimeters
#'
#' @references \url{https://www.cdc.gov/growthcharts/who_charts.htm}
#'
get_lms <-
  function(
    metric = c(  "bmi_for_age"
               , "stature_for_age"
               , "weight_for_age"
               , "weight_for_stature"
               , "head_circumference_for_age"
              )
  , male
  , source = c("CDC-WHO", "WHO", "CDC")
  , age = NA_real_
  , stature = NA_real_
  ) {

  metric <- match.arg(arg = metric, several.ok = FALSE)
  source <- match.arg(arg = source, several.ok = FALSE)
  stopifnot(length(male) == 1L, isTRUE(all.equal(male, 1)) | isTRUE(all.equal(male, 0)))

  lms_data[[metric]] |> names()

  # look for possible data sources for the given metric
  # possible_sources <- names(lms_data[[metric]])
  # if (is.null(possible_sources)) {
  #   stop(sprintf("No data source(s) for metric %s", metric))
  # }
  # if (source == "CDC-WHO") {
  #   if (all(c("CDC", "WHO") %in% possible_sources)) {
  #     # do nothing
  #   } else if ("CDC" %in% possible_sources & !("WHO" %in% possible_sources)) {
  #     source <- "CDC"
  #   } else if ("WHO" %in% possible_sources & !("CDC" %in% possible_sources)) {
  #     source <- "WHO"
  #   } else {
  #     stop("I don't know how you got to this error, but here you are.  Contact the developers with a reproducable example.")
  #   }
  # } else {
  #   if (!isTRUE(source %in% possible_sources)) {
  #     stop(sprintf("For metric %s, you need to select a source from: %s", metric, paste(possible_sources, collapse = ", ")))
  #   }
  # }


  if (grepl("_for_age", metric)) {
    stopifnot(!is.null(age), length(age) == 1L, age >= 0)

    if (source == "CDC-WHO") {
      if (age < 24) {
        source <- "WHO"
      } else {
        source <- "CDC"
      }
    }

    LMS <- lms_data[[metric]][[source]][[male + 1L]]

    if (!is.null(LMS)) {

      age_range <- range(LMS[["age"]])

      if (age < age_range[1]) {
        warning(sprintf("age: %s, is below the min value in the look up table.", age))
        age <- -Inf # this will result in no viable index
      } else if (age > age_range[2]) {
        warning(sprintf("age: %s, is above the max value in the look up table.", age))
        age <- -Inf # this will result in no viable index
      }

      LMS <- LMS[LMS[["age"]] <= age, ]

    } else {
      LMS <- data.frame(L = NA_real_, M = NA_real_, S = NA_real_)
    }

  } else if (grepl("_for_stature", metric)) {
    stopifnot(!is.null(stature), length(stature) == 1L, stature >= 0)

    if (source == "CDC-WHO") {
      cLMS <- lms_data[[metric]][["CDC"]][[male + 1L]]
      if (nrow(cLMS) > 0) {
        source <- "CDC"
      } else {
        source <- "WHO"
      }
    }

    LMS <- lms_data[[metric]][[source]][[male + 1L]]

    if (!is.null(LMS)) {
      stature_range <- range(lms_data[[metric]][[source]][[male + 1L]][["stature"]])

      if (stature < stature_range[1]) {
        warning(sprintf("stature: %s, is below the min value in the look up table", stature))
        stature <- -Inf
      } else if (stature > stature_range[2]) {
        warning(sprintf("stature: %s, is above the max value in the look up table", stature))
        stature <- -Inf
      }
      LMS <- LMS[LMS[["stature"]] <= stature, ]
    } else {
      LMS <- data.frame(L = NA_real_, M = NA_real_, S = NA_real_)
    }

  } else {
    stop("what else could there be except for _for_age and _for_stature?")
  }

  if (nrow(LMS) > 1L) {
    rtn <- tail(LMS, n = 1)[, c("L", "M", "S")]
  } else if (nrow(LMS) == 1L) {
    rtn <- LMS[, c("L", "M", "S")]
  } else {
    rtn <- data.frame(L = NA_real_, M = NA_real_, S = NA_real_)
  } 
  rownames(rtn) <- NULL
  rtn
}

# v_get_lms is a vectorized version of get_lms.  get_lms uses a look up table
# and it was easier to think with single values there and vectorize at a higher
# level.
v_get_lms <-
  function(
    metric = c(  "bmi_for_age"
               , "stature_for_age"
               , "weight_for_age"
               , "weight_for_stature"
               , "head_circumference_for_age"
              )
    , male
    , source = c("CDC-WHO", "WHO", "CDC")
    , age = NA_real_
    , stature = NA_real_
    ) {

  metric <- match.arg(arg = metric, several.ok = FALSE)
  source <- match.arg(arg = source, several.ok = TRUE)

  rtn <-
    mapply(get_lms
           , metric = metric
           , male = male
           , source = source
           , age = age
           , stature = stature
           , SIMPLIFY = FALSE
    )
  rtn <- do.call(rbind, rtn)
  rownames(rtn) <- NULL
  rtn
}

#' Distribution, Quantile, and Zscores by LMS values
#'
#' Non-exported functions ...
#'
#' @param x quantile or percentile value
#' @param l,m,s the lms values
#' @param ... pass through

zlms <- function(x, l, m, s, ...) {
  # stopifnot(all(s >= 0))
  # print(c(l, m, s))
  stopifnot(length(x) == 1L,
            length(l) == 1L,
            length(m) == 1L,
            length(s) == 1L)

  if (isTRUE(all.equal(0.0, l))) {
    z <- log( x / m) / s
  } else {
    z <- ( ((x / m) ^ l) - 1 ) / ( l * s)
  }
  z
}

plms <- function(x, l, m, s, ...) {
  stopifnot(length(x) == 1L,
            length(l) == 1L,
            length(m) == 1L,
            length(s) == 1L)
  z <- zlms(x, l, m, s)
  stats::pnorm(z, mean = 0, sd = 1)
}

qlms <- function(x, l, m, s, ...) {
  # stopifnot(all(s >= 0))
  stopifnot(length(x) == 1L,
            length(l) == 1L,
            length(m) == 1L,
            length(s) == 1L)

  z <- stats::qnorm(x, mean = 0, sd = 1)

  if (isTRUE(all.equal(0.0, l))) {
    rtn <- m * exp(s * z)
  } else {
    rtn <- m * (1 + l * s * z) ^ (1 / l)
  }
  rtn
}
