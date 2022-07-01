# pedbp 1.0.0 (resubmission)

## Testing Results

- Local testing R 4.2.1 - 1 Note (new submission)
- Github actions all passed
  - Windows R-release
  - MacOS   R-release
  - Ubuntu  R-release
  - Ubuntu  R-devel
  - Ubuntu  R-oldrelease

- win-bulider.r-project.org (1 Note)

  New submission

  Possibly misspelled words in DESCRIPTION:
  Gemelli (12:77)
  NHLBI (15:62)
  al (13:8, 17:49, 19:38)
  et (13:5, 17:46, 19:35)

  - Gemelli is the correct spelling of the lead author of a cited paper.
  - NHLBI is the correct acronym for the National Heart, Lung, and Blood Institute
  - The abbreviation "et.al." is used in the description resulting in
    "al" and "et" being flagged as possibly misspelled words.


## Responses to feedback from initial submission

_Comment:_

> The Description field is intended to be a (one paragraph) description of what
> the package does and why it may be useful. Please add more details about the
> package functionality and implemented methods in your Description text.
>
> If there are references describing the methods in your package, please
> add these in the description field of your DESCRIPTION file in the form
> authors (year) <doi:...>
> authors (year) <arXiv:...>
> authors (year, ISBN:...)
> or if those are not available: <https:...>
> with no space after 'doi:', 'arXiv:', 'https:' and angle brackets for
> auto-linking.
> (If you want to add a title as well please put it in quotes: "Title")

  _Response:_

  The Description field has been extended to report data sources and methods.

_Comment:_
> Please add \value to .Rd files regarding exported methods and explain
> the functions results in the documentation. Please write about the
> structure of the output (class) and also what the output means. (If a
> function does not return a value, please document that too, e.g.
> \value{No return value, called for side effects} or similar)
> Missing Rd-tags:
>       bp_cdf.Rd: \value
>       bp_distribution.Rd: \value
>       est_norm.Rd: \value
>       pediatric_vital_sign_distributions.Rd: \value

  _Response:_

  Thank you for noting the missing \value sections.  Said sections have been
  added to the noted Rd-tags.


_Comment:_
> Please ensure that your functions do not write by default or in your
> examples/vignettes/tests in the user's home filespace (including the
> package directory and getwd()). This is not allowed by CRAN policies.
> Please omit any default path in writing functions. In your
> examples/vignettes/tests you can write to tempdir().

  _Response:_
  The only place in the package code which writes to disk is within a function
  in the inst/shinyapps/pedbp/server.R script.  The write occurs only after an
  end user clicks on a download button in an active shiny application. The file
  would then be written to disk based on the settings of the web bowser the
  end user is working in.

  If there is another place in the examples/vignettes/tests where a write is
  occurring that is likely an error and I would be very appreciative if such a
  case could be explicitly identified.

_Comment:_
> Please always make sure to reset to user's options(), working directory
> or par() after you changed it in examples and vignettes and demos. ->
> inst/doc/bp-distributions.R
> e.g.:
> oldpar <- par(mfrow = c(1,2))
> ...
> par(oldpar)

  _Response:_

  The use of `par` has been removed from the vignette.  There is no longer a
  need to reset user's options().

_Comment:_
> Please fix and resubmit.

  _Response:_
  Done.

