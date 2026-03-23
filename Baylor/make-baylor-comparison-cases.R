library(pedbp)

script_path <- NULL
for (i in rev(seq_along(sys.frames()))) {
  if (!is.null(sys.frames()[[i]]$ofile)) {
    script_path <- normalizePath(sys.frames()[[i]]$ofile)
    break
  }
}
script_dir <- if (is.null(script_path)) "tools" else dirname(script_path)

measurement_date <- as.Date("2026-01-06")
age_years <- c(3L, 5L, 8L, 11L, 14L)
sex_map <- data.frame(
  sex = c("Female", "Male"),
  male = c(0L, 1L),
  stringsAsFactors = FALSE
)
scenarios <- data.frame(
  scenario = c("low", "mid", "high"),
  target_height_percentile = c(10, 50, 90),
  target_bp_percentile = c(10, 50, 90),
  stringsAsFactors = FALSE
)

grid <- merge(
  merge(
    data.frame(age_years = age_years, stringsAsFactors = FALSE),
    sex_map,
    all = TRUE
  ),
  scenarios,
  all = TRUE
)

grid <- grid[order(grid$age_years, grid$male, grid$target_bp_percentile), ]
grid$case_id <- sprintf("bcm-%02d", seq_len(nrow(grid)))
grid$mdate <- format(measurement_date, "%Y-%m-%d")
grid$dob <- sprintf("%04d-01-06", as.integer(format(measurement_date, "%Y")) - grid$age_years)
grid$age_months <- grid$age_years * 12
grid$units <- "Metric (cm,kg)"

grid$height <- mapply(
  function(p, male, age_months) {
    q_height_for_age(p = p / 100, male = male, age = age_months, source = "CDC")
  },
  p = grid$target_height_percentile,
  male = grid$male,
  age_months = grid$age_months
)

bp_values <- mapply(
  function(p, male, age_months, height) {
    q_bp(
      p_sbp = p / 100,
      p_dbp = p / 100,
      age = age_months,
      male = male,
      height = height,
      source = "flynn2017"
    )
  },
  p = grid$target_bp_percentile,
  male = grid$male,
  age_months = grid$age_months,
  height = grid$height,
  SIMPLIFY = FALSE
)

grid$sbp <- vapply(bp_values, function(x) x$sbp, numeric(1))
grid$dbp <- vapply(bp_values, function(x) x$dbp, numeric(1))

grid$pedbp_height_percentile <- mapply(
  function(height, male, age_months) {
    100 * p_height_for_age(q = height, male = male, age = age_months, source = "CDC")
  },
  height = grid$height,
  male = grid$male,
  age_months = grid$age_months
)

pedbp_check <- mapply(
  function(sbp, dbp, male, age_months, height) {
    p_bp(
      q_sbp = sbp,
      q_dbp = dbp,
      age = age_months,
      male = male,
      height = height,
      source = "flynn2017"
    )
  },
  sbp = grid$sbp,
  dbp = grid$dbp,
  male = grid$male,
  age_months = grid$age_months,
  height = grid$height,
  SIMPLIFY = FALSE
)

grid$pedbp_sbp_percentile <- 100 * vapply(pedbp_check, function(x) x$sbp_p, numeric(1))
grid$pedbp_dbp_percentile <- 100 * vapply(pedbp_check, function(x) x$dbp_p, numeric(1))

grid <- grid[
  ,
  c(
    "case_id",
    "scenario",
    "dob",
    "mdate",
    "age_years",
    "age_months",
    "sex",
    "male",
    "units",
    "target_height_percentile",
    "target_bp_percentile",
    "height",
    "sbp",
    "dbp",
    "pedbp_height_percentile",
    "pedbp_sbp_percentile",
    "pedbp_dbp_percentile"
  )
]

write.csv(grid, file = file.path(script_dir, "baylor_comparison_cases.csv"), row.names = FALSE)
