# Pediatric Blood Pressure

Pediatric Blood Pressure quantile and probability (distribution
function) values

## Usage

``` r
cppBP(
  qp_sbp,
  qp_dbp,
  age,
  male,
  height,
  height_percentile,
  default_height_percentile,
  source,
  type
)
```

## Arguments

- qp_sbp:

  the quantile(s) or probability(s) for systolic blood pressure

- qp_dbp:

  the quantile(s) or probability(s) for diastolic blood pressure

- age:

  numeric vector, in months

- male:

  integer vector; 0 = female, 1 = male

- height:

  numeric vector of stature

- height_percentile:

  numeric vector for height percentiles, expected values between 0
  and 100. That is, 0.95 would be the 0.95th percentile, and 95 is the
  95th percentile.

- default_height_percentile:

  default height percentile to use if `height` is missing

- source:

  the method, or data set, to use as the reference.

- type:

  quantile or distribution to return

## Value

A list:

\[\[1\]\] systolic blood pressure quantiles or probability (defined by
the input value of `type`). \[\[2\]\] diastolic blood pressure quantiles
or probability (defined by the input value of `type`).

`attr(, "bp_params")` is a `data.frame` with the values for the look up
table(s) needed to inform the sbp and dbp values.

## Details

`height` is used preferentially over `height_percentile` over
`default_height_percentile`.

`source` can be one of `"gemelli1990"`, `"lo2013"`, `"nhlbi"`,
`"flynn2017"`, or `"martin2022"`.
