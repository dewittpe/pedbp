script_path <- NULL
for (i in rev(seq_along(sys.frames()))) {
  if (!is.null(sys.frames()[[i]]$ofile)) {
    script_path <- normalizePath(sys.frames()[[i]]$ofile)
    break
  }
}
script_dir <- if (is.null(script_path)) "tools" else dirname(script_path)

source(file.path(script_dir, "baylor-bcm-bp.R"))

input_path <- file.path(script_dir, "baylor_comparison_cases.csv")
output_path <- file.path(script_dir, "baylor_comparison_cases_results.csv")

cases <- utils::read.csv(input_path, stringsAsFactors = FALSE)
results <- bcm_bp_query_many(cases, progress = TRUE)

merged <- merge(
  cases,
  results[, c(
    "dob",
    "mdate",
    "sex",
    "units",
    "height",
    "sbp",
    "dbp",
    "height_percentile",
    "sbp_percentile",
    "dbp_percentile",
    "status"
  )],
  by = c("dob", "mdate", "sex", "units", "height", "sbp", "dbp"),
  all.x = TRUE,
  sort = FALSE
)

merged$baylor_height_percentile <- merged$height_percentile
merged$baylor_sbp_percentile <- merged$sbp_percentile
merged$baylor_dbp_percentile <- merged$dbp_percentile

merged$height_percentile <- NULL
merged$sbp_percentile <- NULL
merged$dbp_percentile <- NULL

merged$height_percentile_diff <- merged$baylor_height_percentile - merged$pedbp_height_percentile
merged$sbp_percentile_diff <- merged$baylor_sbp_percentile - merged$pedbp_sbp_percentile
merged$dbp_percentile_diff <- merged$baylor_dbp_percentile - merged$pedbp_dbp_percentile

utils::write.csv(merged, file = output_path, row.names = FALSE)
cat("Wrote", output_path, "with", nrow(merged), "results\n")
