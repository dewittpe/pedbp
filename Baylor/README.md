# Baylor Comparison Tools

This directory contains exploratory and supporting code for comparing `pedbp`
against the Baylor College of Medicine pediatric blood pressure calculator:

- https://www.bcm.edu/bodycomplab/BPappZjs/BPvAgeAPPz.html

These files are developer tools, not package tests. They depend on an external
website and, for live queries, a local browser automation session via
`chromote`.

## Files

- `baylor-bcm-bp.R`
  Functions for querying the Baylor calculator from R with `chromote`.
- `make-baylor-comparison-cases.R`
  Generates a compact 30-case comparison panel from `pedbp`.
- `run-baylor-comparison-cases.R`
  Runs the 30-case panel against Baylor and writes the raw results CSV.
- `baylor_comparison_cases.csv`
  The current 30-case input panel.
- `baylor_comparison_cases_results.csv`
  Baylor outputs collected for the current panel.
- `query.R`
  Investigation script comparing Baylor outputs against `pedbp`.
- `test-vs-baylor-college-of-medicine.R`
  Older exploratory work retained for reference.

## Current Findings

The Baylor match is generally reasonable, but a small number of cases differ by
more than five percentile points.

The current investigation points to two separate sources of disagreement:

1. Baylor height percentiles do not always match `pedbp` CDC height percentiles
for the same `dob`, `mdate`, sex, and height.
2. Even when `pedbp` is rerun using Baylor's returned height percentile, several
BP mismatches remain, so the difference is not explained by height alone.

The clearest height example is case `bcm-29` in `baylor_comparison_cases.csv`:
the height was generated as the CDC 50th percentile at 168 months, but Baylor
returned a height percentile of `68.3`.

The largest BP discrepancies are concentrated in a small set of male mid cases
and a few low DBP cases, including:

- `bcm-05`
- `bcm-11`
- `bcm-17`
- `bcm-22`
- `bcm-23`
- `bcm-29`

Rounding the entered height and BP values does not materially improve the fit.

## Working Interpretation

The Baylor calculator says it reflects the AAP/Flynn 2017 guideline and cites
Rosner 2008 on the page. `pedbp`, by contrast, uses the packaged BP lookup
tables and nearest stored height-percentile rows for the `flynn2017` and
`nhlbi` sources.

That means Baylor should be treated as an external comparator, not as an exact
oracle for `pedbp`. A tolerance-based comparison with documented exceptions is
more appropriate than an exact equality test.

## Typical Workflow

From the repo root:

```r
source("tools/baylor-bcm-bp.R")
source("tools/run-baylor-comparison-cases.R")
source("tools/query.R")
```

Or regenerate the input panel first:

```r
source("tools/make-baylor-comparison-cases.R")
source("tools/run-baylor-comparison-cases.R")
source("tools/query.R")
```

## Notes

- `query.R` currently compares Baylor against `pedbp` using `flynn2017`,
  `nhlbi`, and `flynn2017` with Baylor's returned height percentile.
- The Baylor site returns rounded percentile outputs, so small differences are
  expected even when the underlying methods are close.
