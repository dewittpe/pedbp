# Pediatric Growth Standards

Pediatric growth standard based on LMS data from the CDC and WHO.

## Usage

``` r
cppPGSF(metric, source, male, x, qp, type)
```

## Arguments

- metric:

  string, for example bmi_for_age

- source:

  string, CDC or WHO

- male:

  integer, 0 = female; 1 = male

- x:

  is the age (in months), length (cm) or height (cm) as needed for the
  metric.

- qp:

  the quantile or percentile, whichever is relevant for the type

- type:

  quantile, distribution, or zscore

## Details

expect to call this from R after checking some functional arguments
within R.
