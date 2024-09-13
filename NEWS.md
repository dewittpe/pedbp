# pedbp 2.0.1

## Bug Fixes

* improve testing suite to address changes in r-devel
* fix file paths for graphics in vignettes

# pedbp 2.0.0

## Potential Breaking Changes

* API change to the growth standard distribution functions
  * this includes a name change from "vital signs" to "growth standards"
  * complete redo for how the methods are implemented
  * extended data sources

* Use of the terms "percentile", "distribution", and "probability" are more
  robust.  In v1 "percentile" could have referred to any of those terms.  Now,
  percentile implies a value between 0 and 100, distribution (value) or
  probability, denoted with a p, are values between 0 and 1.


## Additions and Major Changes

* Added features to blood pressure distribution functions
  * functions gain the argument `source` to specify the reference values used
    to generate the percentiles.  Default to `martin2022` to keep the same
    functionality as version 1.
  * Add a new source of blood pressure percentiles called `flynn2017` referencing
    Flynn, Joseph T., David C. Kaelber, Carissa M. Baker-Smith, Douglas Blowey,
    Aaron E. Carroll, Stephen R. Daniels, Sarah D. De Ferranti et al. "Clinical
    practice guideline for screening and management of high blood pressure in
    children and adolescents." Pediatrics 140, no. 3 (2017).
  * Implementation has be moved into c++ and the speed improvement compared to
    version 1 is huge.  v2 is more than 200 times faster than v1.

* Add `z_bp` to return z-scores for blood pressure; a complement to the
  existing `q_bp` and `p_bp` methods.

* Create S3 method for `bp_cdf` plotting [#9](https://github.com/dewittpe/pedbp/issues/9)

* Extended the growth chart distribution data source to include the CDC (v1.0.0)
  and World Health Organization (WHO)

* New "growth-standards" vignette

* Add `gs_chart` for building growth standard charts

* Add `gs_cdf` for building growth standard cumulative distribution functions

* Add `bp_chart` for building blood pressure charts

* Rebuild of the Shiny application to use the new features of v2.0.0

## Improvements

* `est_norm` calls `stats::optim` with `method = "L-BFGS-B"`, `lower = c(-Inf, 0)`, and `upper = c(Inf, Inf))` to make sure the standard deviation estimate is non-negative.
* `est_norm` checks for sorted inputs.

# pedbp 1.0.2

* Bug Fixes:
  - Warning when age and height is out of the range in the provided data sets
    instead of error.  This fixes (#5).  The logic is still good.  The known bug
    was when 35.5 < age < 36 for length_by_age_inf which only has age up to 35.5
    but the logic for use of that set is age < 36.  A warning will not be given
    if 35.5 < age < 36.


# pedbp 1.0.1

* Add citation information

# pedbp 1.0.0

* Initial Version for Review of Collaborators

* User Level Features:

  * blood pressure distributions functions
  * CDC growth chart/distribution functions
  * shiny app for blood pressure distribution exploration

