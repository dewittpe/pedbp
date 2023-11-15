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

  # look for possible data sources for the given metric
  possible_sources <- unique(lms_data[["source"]][lms_data[["metric"]] == metric])
  if (is.null(possible_sources)) {
    stop(sprintf("No data source(s) for metric %s", metric))
  }
  if (source == "CDC-WHO") {
    if (all(c("CDC", "WHO") %in% possible_sources)) {
      # do nothing
    } else if ("CDC" %in% possible_sources & !("WHO" %in% possible_sources)) {
      source <- "CDC"
    } else if ("WHO" %in% possible_sources & !("CDC" %in% possible_sources)) {
      source <- "WHO"
    } else {
      stop("I don't know how you got to this error, but here you are.  Contact the developers with a reproducable example.")
    }
  } else {
    if (!isTRUE(source %in% possible_sources)) {
      stop(sprintf("For metric %s, you need to select a source from: %s", metric, paste(possible_sources, collapse = ", ")))
    }
  }

  # start finding the row index for the given inputs
  idx <-
    (lms_data[["metric"]] == metric) &
    (lms_data[["male"]] == male)

  if (grepl("_for_age", metric)) {
    stopifnot(!is.null(age), length(age) == 1L, age >= 0)

    if (source == "CDC-WHO") {
      if (age < 24) {
        idx <- idx & (lms_data[["source"]] == "WHO")
      } else {
        idx <- idx & (lms_data[["source"]] == "CDC")
      }
    } else {
      idx <- idx & (lms_data[["source"]] == source)
    }

    if (!any(idx)) {
      stop(sprintf("no rows in the lms_data look table where found for metric: %s, male: %s, source: %s", metric, as.character(male), source))
    }

    min_age <- min(lms_data[["age"]][idx])
    max_age <- max(lms_data[["age"]][idx])

    if (age < min_age) {
      warning(sprintf("age: %s, is below the min value in the look up table.", age))
      age <- -Inf # this will result in no viable index
    } else if (age > max_age) {
      warning(sprintf("age: %s, is above the max value in the look up table.", age))
      age <- -Inf # this will result in no viable index
    }

    idx <- idx & (lms_data[["age"]] <= age)

  } else if (grepl("_for_stature", metric)) {
    stopifnot(!is.null(stature), length(stature) == 1L, stature >= 0)

    min_stature <- min(lms_data[["stature"]][idx])
    max_stature <- max(lms_data[["stature"]][idx])

    if (stature < min_stature) {
      message(sprintf("stature: %s, is below the min value in the look up table", stature))
      stature <- -Inf
    } else if (stature > max_stature) {
      message(sprintf("stature: %s, is above the max value in the look up table", stature))
      stature <- -Inf
    }

    idx <- idx & (lms_data[["stature"]] <= stature)

    if (source == "CDC-WHO") {
      cidx <- lms_data[["source"]][idx] == "CDC"
      widx <- lms_data[["source"]][idx] == "CDC"
      if (any(cidx)) {
        idx <- idx & lms_data[["source"]] == "CDC"
      } else {
        idx <- idx & lms_data[["source"]] == "WHO"
      }
    } else {
      idx <- idx & (lms_data[["source"]] == source)
    }

  } else {
    stop("what else could there be except for _for_age and _for_stature?")
  }

  if (any(idx)) {
    rtn <- lms_data[max(which(idx)), c("L", "M", "S")]
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
