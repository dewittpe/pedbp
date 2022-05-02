PKG_ROOT    = .
PKG_VERSION = $(shell gawk '/^Version:/{print $$2}' $(PKG_ROOT)/DESCRIPTION)
PKG_NAME    = $(shell gawk '/^Package:/{print $$2}' $(PKG_ROOT)/DESCRIPTION)

CRAN = "https://cran.rstudio.com"

# General Package Dependencies
SRC       = $(wildcard $(PKG_ROOT)/src/*.cpp)
RFILES    = $(wildcard $(PKG_ROOT)/R/*.R)
TESTS     = $(wildcard $(PKG_ROOT)/tests/*.R)

# Targets
#
## Vignettes
# These are both targets for building and dependencies for the package tar.gz
# file
VIGNETTES  = $(PKG_ROOT)/vignettes/bp-distributions.Rmd

## Data targets
DATATARGETS  = $(PKG_ROOT)/data/lo2013.rda
DATATARGETS += $(PKG_ROOT)/data/gemelli1990.rda
DATATARGETS += $(PKG_ROOT)/data/nhlbi_bp_norms.rda
DATATARGETS += $(PKG_ROOT)/data/bp_parameters.rda
DATATARGETS += $(PKG_ROOT)/R/sysdata.rda

################################################################################
# Recipes

.PHONY: all check install clean

all: $(PKG_NAME)_$(PKG_VERSION).tar.gz

test:
	${assert}

$(PKG_NAME)_$(PKG_VERSION).tar.gz: .install_dev_deps.Rout .document.Rout $(VIGNETTES) $(TESTS) $(DATATARGETS)
	R CMD build --md5 $(build-options) $(PKG_ROOT)

.install_dev_deps.Rout : $(PKG_ROOT)/DESCRIPTION
	Rscript --vanilla --quiet -e "options(repo = c('$(CRAN)'))" \
		-e "if (!require(devtools)) {install.packages('devtools', repo = c('$(CRAN)'))}" \
		-e "options(warn = 2)" \
		-e "devtools::install_dev_deps()"
	touch $@

.document.Rout: $(SRC) $(RFILES) $(DATATARGETS) $(EXAMPLES) $(PKG_ROOT)/DESCRIPTION
	Rscript --vanilla --quiet -e "options(warn = 2)" \
		-e "devtools::document('$(PKG_ROOT)')"
	touch $@

################################################################################
# Recipes for Vignettes
#
# Expecting that the vignettes are built via knitr::spin
#
# List the explicit targets above

$(PKG_ROOT)/vignettes/%.Rmd : $(PKG_ROOT)/vignette-spinners/%.R
	R --vanilla --quiet -e "knitr::spin(hair = '$<', knit = FALSE)"
	mv $(basename $<).Rmd $@

################################################################################
# Data Sets
#
$(PKG_ROOT)/data/nhlbi_bp_norms.rda : data-raw/nhlbi_bp_norms.R data-raw/nhlbi_bp_norms_boys.csv data-raw/nhlbi_bp_norms_girls.csv
	R CMD BATCH --vanilla $<

$(PKG_ROOT)/data/lo2013.rda : data-raw/lo2013.R data-raw/lo2013_bp_weight_height_bmi.txt
	R CMD BATCH --vanilla $<

$(PKG_ROOT)/data/gemelli1990.rda : data-raw/gemelli1990.R data-raw/gemelli1990_female.csv data-raw/gemelli1990_male.csv
	R CMD BATCH --vanilla $<

$(PKG_ROOT)/data/bp_parameters.rda : data-raw/gaussian_parameters.R R/est_norm.R data/gemelli1990.rda  data/nhlbi_bp_norms.rda data/lo2013.rda
	R CMD BATCH --vanilla $<

$(PKG_ROOT)/R/sysdata.rda : data-raw/sysdata.R data-raw/cdc_percentile_data_with_lms_values/bmiagerev.csv data-raw/cdc_percentile_data_with_lms_values/hcageinf.csv data-raw/cdc_percentile_data_with_lms_values/lenageinf.csv data-raw/cdc_percentile_data_with_lms_values/statage.csv data-raw/cdc_percentile_data_with_lms_values/wtageinf.csv data-raw/cdc_percentile_data_with_lms_values/wtage.csv data-raw/cdc_percentile_data_with_lms_values/wtleninf.csv data-raw/cdc_percentile_data_with_lms_values/wtstat.csv
	R CMD BATCH --vanilla $<

################################################################################
# Other Recipes for checking the package, (un)installing, and cleaning the
# working directory.
#
covr-report.html : $(PKG_NAME)_$(PKG_VERSION).tar.gz
	R --vanilla --quiet -e 'x <- covr::package_coverage(type = "tests")'\
		-e 'covr::report(x, file = "covr-report.html")'

check: $(PKG_NAME)_$(PKG_VERSION).tar.gz
	R CMD check $(PKG_NAME)_$(PKG_VERSION).tar.gz

check-as-cran: $(PKG_NAME)_$(PKG_VERSION).tar.gz
	R CMD check --as-cran $(PKG_NAME)_$(PKG_VERSION).tar.gz

install: $(PKG_NAME)_$(PKG_VERSION).tar.gz
	R CMD INSTALL $(PKG_NAME)_$(PKG_VERSION).tar.gz

uninstall :
	R --vanilla --quiet -e "try(remove.packages('pedalfast.data'), silent = TRUE)"

clean:
	$(RM) -f  $(PKG_NAME)_$(PKG_VERSION).tar.gz
	$(RM) -rf $(PKG_NAME).Rcheck
	$(RM) -f .*.Rout
	$(RM) -f *.Rout

