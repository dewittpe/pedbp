library(pedbp)

script_path <- NULL
for (i in rev(seq_along(sys.frames()))) {
  if (!is.null(sys.frames()[[i]]$ofile)) {
    script_path <- normalizePath(sys.frames()[[i]]$ofile)
    break
  }
}
script_dir <- if (is.null(script_path)) "tools" else dirname(script_path)

source(file.path(script_dir, "baylor-bcm-bp.R"))
d <- read.csv(file.path(script_dir, "baylor_comparison_cases.csv"))

# rtn <-
#   bcm_bp_query_many(
#     d[, c("dob", "mdate", "sex", "units", "height", "sbp", "dbp")]
#   )
# write.csv(rtn, file = file.path(script_dir, "baylor_comparison_cases_results.csv"))
baylor <- read.csv(file = file.path(script_dir, "baylor_comparison_cases_results.csv"))

outbp_flynn <-
  p_bp(
    q_sbp = d$sbp,
    q_dbp = d$dbp, 
    age = d$age_months,
    male = d$male,
    height = d$height,
    source = "flynn2017"
  )

outbp_nhlbi <-
  p_bp(
    q_sbp = d$sbp,
    q_dbp = d$dbp,
    age = d$age_months,
    male = d$male,
    height = d$height,
    source = "nhlbi"
  )

outht <- 
  p_height_for_age(
    q = d$height,
    age = d$age_months,
    male = d$male
  )

baylor_height_percentile <- as.numeric(baylor$raw_height_percentile)

outbp_flynn_baylor_height <-
  p_bp(
    q_sbp = d$sbp,
    q_dbp = d$dbp,
    age = d$age_months,
    male = d$male,
    height = NA_real_,
    height_percentile = baylor_height_percentile,
    source = "flynn2017"
  )

pedbp_vs_baylor <- 
  cbind(
    d,
    baylor_height_z = as.numeric(baylor$raw_height_z),
    baylor_height_p = as.numeric(baylor$raw_height_percentile) / 100,
    baylor_sbp_p    = as.numeric(baylor$raw_sbp_percentile) / 100,
    baylor_dbp_p    = as.numeric(baylor$raw_dbp_percentile) / 100,
    pedbp_height_p  = outht,
    pedbp_flynn_sbp_p = outbp_flynn$sbp_p,
    pedbp_flynn_dbp_p = outbp_flynn$dbp_p,
    pedbp_nhlbi_sbp_p = outbp_nhlbi$sbp_p,
    pedbp_nhlbi_dbp_p = outbp_nhlbi$dbp_p,
    pedbp_flynn_sbp_p_at_baylor_height_p = outbp_flynn_baylor_height$sbp_p,
    pedbp_flynn_dbp_p_at_baylor_height_p = outbp_flynn_baylor_height$dbp_p
  )

pedbp_vs_baylor$delta_height_p <- with(pedbp_vs_baylor, pedbp_height_p - baylor_height_p)
pedbp_vs_baylor$delta_flynn_sbp_p <- with(pedbp_vs_baylor, pedbp_flynn_sbp_p - baylor_sbp_p)
pedbp_vs_baylor$delta_flynn_dbp_p <- with(pedbp_vs_baylor, pedbp_flynn_dbp_p - baylor_dbp_p)
pedbp_vs_baylor$delta_nhlbi_sbp_p <- with(pedbp_vs_baylor, pedbp_nhlbi_sbp_p - baylor_sbp_p)
pedbp_vs_baylor$delta_nhlbi_dbp_p <- with(pedbp_vs_baylor, pedbp_nhlbi_dbp_p - baylor_dbp_p)
pedbp_vs_baylor$delta_flynn_sbp_p_at_baylor_height_p <- with(pedbp_vs_baylor, pedbp_flynn_sbp_p_at_baylor_height_p - baylor_sbp_p)
pedbp_vs_baylor$delta_flynn_dbp_p_at_baylor_height_p <- with(pedbp_vs_baylor, pedbp_flynn_dbp_p_at_baylor_height_p - baylor_dbp_p)

with(pedbp_vs_baylor, delta_height_p) |> summary()
with(pedbp_vs_baylor, delta_flynn_sbp_p) |> summary()
with(pedbp_vs_baylor, delta_flynn_dbp_p) |> summary()

pedbp_vs_baylor$flynn_total_abs_error <-
  with(pedbp_vs_baylor, abs(delta_flynn_sbp_p) + abs(delta_flynn_dbp_p))
pedbp_vs_baylor$nhlbi_total_abs_error <-
  with(pedbp_vs_baylor, abs(delta_nhlbi_sbp_p) + abs(delta_nhlbi_dbp_p))

aggregate(
  cbind(
    flynn_total_abs_error,
    nhlbi_total_abs_error
  ) ~ sex + scenario,
  data = pedbp_vs_baylor,
  FUN = mean
)

subset(
  pedbp_vs_baylor,
  abs(delta_flynn_sbp_p) > 0.05 | abs(delta_flynn_dbp_p) > 0.05,
  select = c(
    case_id,
    age_years,
    sex,
    scenario,
    target_height_percentile,
    target_bp_percentile,
    height,
    pedbp_height_p,
    baylor_height_p,
    pedbp_flynn_sbp_p,
    baylor_sbp_p,
    pedbp_nhlbi_sbp_p,
    pedbp_flynn_sbp_p_at_baylor_height_p,
    pedbp_flynn_dbp_p,
    baylor_dbp_p,
    pedbp_nhlbi_dbp_p,
    pedbp_flynn_dbp_p_at_baylor_height_p,
    delta_flynn_sbp_p,
    delta_flynn_dbp_p
  )
)
