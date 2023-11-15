# pedbp 2.0.0

* API change to the growth standard distribution functions
  * this includes a name change from "vital signs" to "growth stardards"
  * complete redo for how the methods are implemented
  * extended data sources

* Extended the growth chart distribution data source to include the CDC (v1.0.0)
  and World Health Organization (WHO)

* New "growth-standards" vignette

## Improvements

* `est_norm` calls `stats::optim` with `method = "L-BFGS-B"`, `lower = c(-Inf, 0)`, and `upper = c(Inf, Inf))` to make sure the sd estimate is non-negative.
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

