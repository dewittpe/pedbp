# pedbp 2.0.0

* API change to the vital sign distribution functions

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

