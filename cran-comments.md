# pedbp 2.0.1

Fixes to the testing suite to address the errors resulting from a change in
R-devel

> r87097 | maechler | 2024-09-05 12:18:24 +0200 (Thu, 05 Sep 2024) | 1 line new R_MissingArgError() + ...Error_c() variant

## Testing Results
- Local Testing and Rhub - all tests either result in Status OK or error due to
  issues with installing imported or suggestest packages.

# pedbp 2.0.0

Major update and reimplementation of the package.

## Testing Results

- Local testing R 4.3.3  (Status OK)

- Github actions (Status OK)
  - Windows R-release
  - MacOS   R-release
  - Ubuntu  R-release
  - Ubuntu  R-devel
  - Ubuntu  R-oldrelease

# pedbp 1.0.2

## Testing Results

- Local testing R 4.2.1

- Github actions all passed
  - Windows R-release
  - MacOS   R-release
  - Ubuntu  R-release
  - Ubuntu  R-devel
  - Ubuntu  R-oldrelease

- R-hub Windows Server 2022, R-devel, 64 bit  (1 Note)

    * checking for detritus in the temp directory ... NOTE
    Found the following files/directories:
      'lastMiKTeXException'

  This note can be ignored, see https://github.com/r-hub/rhub/issues/503
  This note is likely related to a bug/crash in miktex

- win-bulider.r-project.org (Status: OK)


## Notes from prior submissions I want/need to remind myself and reviewers of

On a prior submission the following was found:

  Possibly misspelled words in DESCRIPTION:
  Gemelli (12:77)
  NHLBI (15:62)
  al (13:8, 17:49, 19:38)
  et (13:5, 17:46, 19:35)

  - Gemelli is the correct spelling of the lead author of a cited paper.
  - NHLBI is the correct acronym for the National Heart, Lung, and Blood Institute
  - The abbreviation "et.al." is used in the description resulting in
    "al" and "et" being flagged as possibly misspelled words.


_Comment from version 1.0.0_
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

