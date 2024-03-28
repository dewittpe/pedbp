PKG_ROOT    = .
PKG_VERSION = $(shell gawk '/^Version:/{print $$2}' $(PKG_ROOT)/DESCRIPTION)
PKG_NAME    = $(shell gawk '/^Package:/{print $$2}' $(PKG_ROOT)/DESCRIPTION)

CRAN = "https://cran.rstudio.com"

# General Package Dependencies
SRC       = $(wildcard $(PKG_ROOT)/src/*.cpp)
RFILES    = $(wildcard $(PKG_ROOT)/R/*.R)
TESTS     = $(wildcard $(PKG_ROOT)/tests/*.R)
SHINYAPPS = $(wildcard $(PKG_ROOT)/inst/shinyapps/*)

# Targets
#
## Vignettes
# These are both targets for building and dependencies for the package tar.gz
# file
VIGNETTES  = $(PKG_ROOT)/vignettes/bp-distributions.Rmd
VIGNETTES += $(PKG_ROOT)/vignettes/growth-standards.Rmd
VIGNETTES += $(PKG_ROOT)/vignettes/additional-utilities.Rmd

## Data targets
DATATARGETS  = $(PKG_ROOT)/data/lo2013.rda
DATATARGETS += $(PKG_ROOT)/data/gemelli1990.rda
DATATARGETS += $(PKG_ROOT)/data/nhlbi_bp_norms.rda
DATATARGETS += $(PKG_ROOT)/data/flynn2017.rda
DATATARGETS += $(PKG_ROOT)/data/bp_parameters.rda
DATATARGETS += $(PKG_ROOT)/R/sysdata.rda

## sysdata sources
SYSDATA_SRCS  = $(PKG_ROOT)/data-raw/cdc2000/bmiagerev.csv
SYSDATA_SRCS += $(PKG_ROOT)/data-raw/cdc2000/hcageinf.csv
SYSDATA_SRCS += $(PKG_ROOT)/data-raw/cdc2000/lenageinf.csv
SYSDATA_SRCS += $(PKG_ROOT)/data-raw/cdc2000/statage.csv
SYSDATA_SRCS += $(PKG_ROOT)/data-raw/cdc2000/wtage.csv
SYSDATA_SRCS += $(PKG_ROOT)/data-raw/cdc2000/wtageinf.csv
SYSDATA_SRCS += $(PKG_ROOT)/data-raw/cdc2000/wtleninf.csv
SYSDATA_SRCS += $(PKG_ROOT)/data-raw/cdc2000/wtstat.csv
SYSDATA_SRCS += $(PKG_ROOT)/data-raw/who/wfa-girls-zscores.xlsx
SYSDATA_SRCS += $(PKG_ROOT)/data-raw/who/wfa-girls-percentiles.xlsx
SYSDATA_SRCS += $(PKG_ROOT)/data-raw/who/wfa-boys-zscores.xlsx
SYSDATA_SRCS += $(PKG_ROOT)/data-raw/who/wfa-boys-percentiles.xlsx
SYSDATA_SRCS += $(PKG_ROOT)/data-raw/who/lhfa-girls-zscores.xlsx
SYSDATA_SRCS += $(PKG_ROOT)/data-raw/who/lhfa-girls-percentiles.xlsx
SYSDATA_SRCS += $(PKG_ROOT)/data-raw/who/lhfa-boys-zscores.xlsx
SYSDATA_SRCS += $(PKG_ROOT)/data-raw/who/lhfa-boys-percentiles.xlsx
SYSDATA_SRCS += $(PKG_ROOT)/data-raw/who/wfl-girls-zscores.xlsx
SYSDATA_SRCS += $(PKG_ROOT)/data-raw/who/wfh-girls-zscores.xlsx
SYSDATA_SRCS += $(PKG_ROOT)/data-raw/who/wfl-girls-percentiles.xlsx
SYSDATA_SRCS += $(PKG_ROOT)/data-raw/who/wfh-girls-percentiles.xlsx
SYSDATA_SRCS += $(PKG_ROOT)/data-raw/who/wfl-boys-zscore.xlsx
SYSDATA_SRCS += $(PKG_ROOT)/data-raw/who/wfh-boys-zscores.xlsx
SYSDATA_SRCS += $(PKG_ROOT)/data-raw/who/wfl-boys-percentiles.xlsx
SYSDATA_SRCS += $(PKG_ROOT)/data-raw/who/wfh-boys-percentiles.xlsx
SYSDATA_SRCS += $(PKG_ROOT)/data-raw/who/bfa-girls-zscores.xlsx
SYSDATA_SRCS += $(PKG_ROOT)/data-raw/who/bfa-girls-percentiles.xlsx
SYSDATA_SRCS += $(PKG_ROOT)/data-raw/who/bfa-boys-zscore.xlsx
SYSDATA_SRCS += $(PKG_ROOT)/data-raw/who/bfa-boys-percentiles.xlsx
SYSDATA_SRCS += $(PKG_ROOT)/data-raw/who/hfa-girls-5-19-zscores.xlsx
SYSDATA_SRCS += $(PKG_ROOT)/data-raw/who/hfa-boys-5-19-zscore.xlsx
SYSDATA_SRCS += $(PKG_ROOT)/data-raw/who/hfa-girls-5-19-percentiles.xlsx
SYSDATA_SRCS += $(PKG_ROOT)/data-raw/who/hfa-boys-5-19-percentiles.xlsx
SYSDATA_SRCS += $(PKG_ROOT)/data-raw/who/bfa-girls-5-19-zscore.xlsx
SYSDATA_SRCS += $(PKG_ROOT)/data-raw/who/bfa-boys-5-19-zscore.xlsx
SYSDATA_SRCS += $(PKG_ROOT)/data-raw/who/bfa-girls-5-19-percentiles.xlsx
SYSDATA_SRCS += $(PKG_ROOT)/data-raw/who/bfa-boys-5-19-percentiles.xlsx
SYSDATA_SRCS += $(PKG_ROOT)/data-raw/who/wfa-girls-5-19-zscore.xlsx
SYSDATA_SRCS += $(PKG_ROOT)/data-raw/who/wfa-boys-5-19-zscore.xlsx
SYSDATA_SRCS += $(PKG_ROOT)/data-raw/who/wfa-girls-5-19-percentiles.xlsx
SYSDATA_SRCS += $(PKG_ROOT)/data-raw/who/wfa-boys-5-19-percentiles.xlsx

################################################################################
# Recipes

.PHONY: all check install clean

all: $(PKG_NAME)_$(PKG_VERSION).tar.gz

test:
	${assert}

$(PKG_NAME)_$(PKG_VERSION).tar.gz: .install_dev_deps.Rout .document.Rout $(VIGNETTES) $(TESTS) $(DATATARGETS) $(SHINYAPPS) $(SRC)
	R CMD build --md5 $(build-options) $(PKG_ROOT)

.install_dev_deps.Rout : $(PKG_ROOT)/DESCRIPTION
	Rscript --vanilla --quiet -e "options(repo = c('$(CRAN)'))" \
		-e "if (!require(devtools)) {install.packages('devtools', repo = c('$(CRAN)'))}" \
		-e "options(warn = 2)" \
		-e "devtools::install_dev_deps()"
	touch $@

.document.Rout: $(SRC) $(RFILES) $(DATATARGETS) $(EXAMPLES) $(PKG_ROOT)/DESCRIPTION $(PKG_ROOT)/README.Rmd
	Rscript --vanilla --quiet -e "options(warn = 2)" \
		-e "devtools::document('$(PKG_ROOT)')"\
		-e "knitr::knit('$(PKG_ROOT)/README.Rmd', output = 'README.md')"
	touch $@

################################################################################
# Recipes for Vignettes
#
# Expecting that the vignettes are built via knitr::spin
#
# List the explicit targets above

$(PKG_ROOT)/vignettes/%.Rmd : $(PKG_ROOT)/vignette-spinners/%.R $(PKG_ROOT)/vignettes/references.bib
	R --vanilla --quiet -e "knitr::spin(hair = '$<', knit = FALSE)"
	mv $(basename $<).Rmd $@

################################################################################
# Data Sets
#
$(PKG_ROOT)/data/nhlbi_bp_norms.rda : data-raw/nhlbi_bp_norms.R data-raw/nhlbi_bp_norms_boys.csv data-raw/nhlbi_bp_norms_girls.csv
	R CMD BATCH --vanilla $<

$(PKG_ROOT)/data/flynn2017.rda : data-raw/flynn2017.R data-raw/flynn2017_bp_boys.dat data-raw/flynn2017_bp_girls.dat
	R CMD BATCH --vanilla $<

$(PKG_ROOT)/data/lo2013.rda : data-raw/lo2013.R data-raw/lo2013_bp_weight_height_bmi.txt
	R CMD BATCH --vanilla $<

$(PKG_ROOT)/data/gemelli1990.rda : data-raw/gemelli1990.R data-raw/gemelli1990_female.csv data-raw/gemelli1990_male.csv
	R CMD BATCH --vanilla $<

$(PKG_ROOT)/data/bp_parameters.rda : data-raw/gaussian_parameters.R R/est_norm.R data/gemelli1990.rda  data/nhlbi_bp_norms.rda data/lo2013.rda data/flynn2017.rda
	R CMD BATCH --vanilla $<

$(PKG_ROOT)/R/sysdata.rda : data-raw/sysdata.R $(SYSDATA_SRCS)
	R CMD BATCH --vanilla $<

################################################################################
# Other Recipes for checking the package, (un)installing, and cleaning the
# working directory.
#
covr-report-tests.html : $(PKG_NAME)_$(PKG_VERSION).tar.gz
	R --vanilla --quiet -e 'x <- covr::package_coverage(type = c("tests"), function_exclusions = c("plot\\\\."), line_exclusions = list("R/pedbp-defunct.R", "R/zzz.R"))'\
		-e 'covr::report(x, file = "covr-report-tests.html")'

covr-report-vignettes.html : $(PKG_NAME)_$(PKG_VERSION).tar.gz
	R --vanilla --quiet -e 'x <- covr::package_coverage(type = c("vignettes"), line_exclusions = list("R/pedbp-defunct.R", "R/zzz.R"))'\
		-e 'covr::report(x, file = "covr-report-vignettes.html")'

covr-report-examples.html : $(PKG_NAME)_$(PKG_VERSION).tar.gz
	R --vanilla --quiet -e 'x <- covr::package_coverage(type = c("examples"), line_exclusions = list("R/pedbp-defunct.R", "R/zzz.R"))'\
		-e 'covr::report(x, file = "covr-report-examples.html")'

covr : covr-report-tests.html covr-report-vignettes.html covr-report-examples.html


check: $(PKG_NAME)_$(PKG_VERSION).tar.gz
	R CMD check $(PKG_NAME)_$(PKG_VERSION).tar.gz

check-as-cran: $(PKG_NAME)_$(PKG_VERSION).tar.gz
	R CMD check --as-cran $(PKG_NAME)_$(PKG_VERSION).tar.gz

install: $(PKG_NAME)_$(PKG_VERSION).tar.gz
	R CMD INSTALL $(PKG_NAME)_$(PKG_VERSION).tar.gz

uninstall :
	R --vanilla --quiet -e "try(remove.packages('pedalfast.data'), silent = TRUE)"

shiny: install
	R -e "shiny::runApp(normalizePath(system.file('shinyapps', 'pedbp', package = 'pedbp')), port = 4492)"

clean:
	$(RM) $(PKG_NAME)_$(PKG_VERSION).tar.gz
	$(RM) -r $(PKG_NAME).Rcheck
	$(RM) .*.Rout
	$(RM) *.Rout
	$(RM) *.html
	$(RM) vignettes/*.html
	$(RM) src/*.o
	$(RM) docs
	$(RM) -r lib

