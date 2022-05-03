#' Batch Process a File or Data Frame
#'
#' Given a \code{data.frame} of age, sex, height, and blood pressures, return
#' the \code{data.frame} with two additional columns reporting the percentiles
#' for the systolic and diastolic blood pressures.
#'
#' @param x a \code{data.frame}
#' @param columns named integer vector for identifying the id, age, sex, height,
#' sbp and dpb columns
#' @param ... no currently used
#'
#' @return a \code{data.frame} with the \code{columns} from \code{x} with two
#' additional columns: \code{sbp_percentile} and \code{dbp_percentile}.
#'
#' @examples
#'
#' d <- read.csv(system.file("example_data", "for_batch.csv", package = "pedbp"))
#' d
#' batch1 <- p_bp_batch(d)
#' batch1
#'
#' # use with a data.frame with columns in a different order
#' d2 <- data.frame(
#'                    age = d$age_months
#'                  , x1 = runif(5)
#'                  , sbp = d$sbp..mmHg.
#'                  , ht  = d$height..cm.
#'                  , sex = d$male
#'                  , dbp = d$dbp..mmHg.
#'                  , id  = d$pid
#'                  )
#' d2
#' batch2 <- p_bp_batch(d2, columns = c(idcol = 7, age = 1, male = 5, height = 4, sbp = 3, dbp = 6))
#' batch2
#'
#' # getting quantiles
#' q_bp_batch(batch1, columns = c(idcol = 1, age = 2, male = 3, height = 4, sbp_percentile = 7, dbp_percentile = 8))
#'
#'
#' @name bp_batch
NULL

#' @export
p_bp_batch <- function(x, columns = c(idcol = 1, age = 2, male = 3, height = 4, sbp = 5, dbp = 6), ...) {
  UseMethod("p_bp_batch")
}

#' @export
q_bp_batch <- function(x, columns = c(idcol = 1, age = 2, male = 3, height = 4, sbp_percentile = 5, dbp_percentile = 6), ...) {
  UseMethod("q_bp_batch")
}

#' @export
p_bp_batch.data.frame <- function(x, columns = c(idcol = 1, age = 2, male = 3, height = 4, sbp = 5, dbp = 6), ...) {
  bps <-
    apply(
            X = x[, columns[c("age", "male", "height", "sbp", "dbp")]]
          , MARGIN = 1
          , FUN = function(x) {
            b <-
              p_bp(q_sbp = x[4],
                   q_dbp = x[5],
                   age   = x[1],
                   male  = x[2],
                   height = x[3])
            c(sbp_percentile = unname(b$sbp_percentile), dbp_percentile = unname(b$dbp_percentile))
          }, simplify = FALSE)

  bps <- do.call(rbind, bps)
  cbind(x[, columns[c("idcol", "age", "male", "height", "sbp", "dbp")]], bps)
}

#' @export
q_bp_batch.data.frame <- function(x, columns = c(idcol = 1, age = 2, male = 3, height = 4, sbp_percentile = 5, dbp_percentile = 6), ...) {
  bps <-
    apply(
            X = x[, columns[c("age", "male", "height", "sbp_percentile", "dbp_percentile")]]
          , MARGIN = 1
          , FUN = function(x) {
            b <-
              q_bp(q_sbp = x[4],
                   q_dbp = x[5],
                   age   = x[1],
                   male  = x[2],
                   height = x[3])
            c(sbp = unname(b$sbp), dbp = unname(b$dbp))
          }, simplify = FALSE)

  bps <- do.call(rbind, bps)
  cbind(x[, columns[c("idcol", "age", "male", "height", "sbp_percentile", "dbp_percentile")]], bps)
}

