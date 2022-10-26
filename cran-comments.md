# pedbp 1.0.1

## Testing Results

- Local testing R 4.2.1

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

