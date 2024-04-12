# Vignette Spinners

Vignette spinners are R files which are spun to .Rmd files to be vignettes for
the package or articles for the website.  CRAN wants small packages, and small
vignettes.  But, the website can (should) have longer, more detailed articles.

In this directory there are files with the following naming convention:

    <topic>_CRAN.R
    <topic>_ARTICLE.R

`<topic>_CRAN.R` will be spun to `vignettes/<topic>.Rmd` and
`<topic>_ARTICLE.R` will be spun to `vignettes/articles/<topic>.Rmd`. (See the
package Makefile.

The `vignettes/articles` directory is listed in `.RBuildignore`.

To have pkgdown build the website as expected, the
`.github/workflow/pkgdown.yml` file has been modified to remove the CRAN only
vignettes and publish only the articles.  Suggestion for how to do this was
found on the [pkgdown github](https://github.com/r-lib/pkgdown/issues/2202#issuecomment-1266514749).

    - name: delete-cran-vignettes
    - runs: rm vignettes/additional-utilities.Rmd vignettes/bp-distributions.Rmd vignettes/growth-standards.Rmd

