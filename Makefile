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
VIGNETTES += $(PKG_ROOT)/vignettes/growth-charts.Rmd

## Data targets
DATATARGETS  = $(PKG_ROOT)/data/lo2013.rda
DATATARGETS += $(PKG_ROOT)/data/gemelli1990.rda
DATATARGETS += $(PKG_ROOT)/data/nhlbi_bp_norms.rda
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
SYSDATA_SRCS += $(PKG_ROOT)/data-raw/who/bfa-boys-percentiles-expanded-tables.xlsx?sfvrsn=aec7ec8d_9
SYSDATA_SRCS += $(PKG_ROOT)/data-raw/who/bfa-boys-zscore-expanded-tables.xlsx?sfvrsn=f8e1fbe2_10
SYSDATA_SRCS += $(PKG_ROOT)/data-raw/who/bfa-girls-percentiles-expanded-tables.xlsx?sfvrsn=e9395fe_9
SYSDATA_SRCS += $(PKG_ROOT)/data-raw/who/bfa-girls-zscore-expanded-tables.xlsx?sfvrsn=ae4cb8d1_12
SYSDATA_SRCS += $(PKG_ROOT)/data-raw/who/bmi-boys-perc-who2007-exp.xlsx?sfvrsn=28412fcf_2
SYSDATA_SRCS += $(PKG_ROOT)/data-raw/who/bmi-boys-z-who-2007-exp.xlsx?sfvrsn=a84bca93_2
SYSDATA_SRCS += $(PKG_ROOT)/data-raw/who/bmi-girls-perc-who2007-exp.xlsx?sfvrsn=e866c0a0_2
SYSDATA_SRCS += $(PKG_ROOT)/data-raw/who/bmi-girls-z-who-2007-exp.xlsx?sfvrsn=79222875_2
SYSDATA_SRCS += $(PKG_ROOT)/data-raw/who/hfa-boys-perc-who2007-exp.xlsx?sfvrsn=27f20eb1_2
SYSDATA_SRCS += $(PKG_ROOT)/data-raw/who/hfa-boys-perc-who2007-exp_07eb5053-9a09-4910-aa6b-c7fb28012ce6.xlsx?sfvrsn=97ab852c_4
SYSDATA_SRCS += $(PKG_ROOT)/data-raw/who/hfa-boys-z-who-2007-exp.xlsx?sfvrsn=7fa263d_2
SYSDATA_SRCS += $(PKG_ROOT)/data-raw/who/hfa-boys-z-who-2007-exp_0ff9c43c-8cc0-4c23-9fc6-81290675e08b.xlsx?sfvrsn=b3ca0d6f_4
SYSDATA_SRCS += $(PKG_ROOT)/data-raw/who/hfa-girls-perc-who2007-exp.xlsx?sfvrsn=7a910e5d_2
SYSDATA_SRCS += $(PKG_ROOT)/data-raw/who/hfa-girls-perc-who2007-exp_6040a43e-81da-48fa-a2d4-5c856fe4fe71.xlsx?sfvrsn=5c5825c4_4
SYSDATA_SRCS += $(PKG_ROOT)/data-raw/who/hfa-girls-z-who-2007-exp.xlsx?sfvrsn=79d310ee_2
SYSDATA_SRCS += $(PKG_ROOT)/data-raw/who/hfa-girls-z-who-2007-exp_7ea58763-36a2-436d-bef0-7fcfbadd2820.xlsx?sfvrsn=6ede55a4_4
SYSDATA_SRCS += $(PKG_ROOT)/data-raw/who/lhfa-boys-percentiles-expanded-tables.xlsx?sfvrsn=bc36d818_9
SYSDATA_SRCS += $(PKG_ROOT)/data-raw/who/lhfa-boys-zscore-expanded-tables.xlsx?sfvrsn=7b4a3428_12
SYSDATA_SRCS += $(PKG_ROOT)/data-raw/who/lhfa-girls-percentiles-expanded-tables.xlsx?sfvrsn=478569a5_9
SYSDATA_SRCS += $(PKG_ROOT)/data-raw/who/lhfa-girls-zscore-expanded-tables.xlsx?sfvrsn=27f1e2cb_10
SYSDATA_SRCS += $(PKG_ROOT)/data-raw/who/wfa-boys-percentiles-expanded-tables.xlsx?sfvrsn=c2f79259_11
SYSDATA_SRCS += $(PKG_ROOT)/data-raw/who/wfa-boys-zscore-expanded-tables.xlsx?sfvrsn=65cce121_10
SYSDATA_SRCS += $(PKG_ROOT)/data-raw/who/wfa-girls-percentiles-expanded-tables.xlsx?sfvrsn=54cfa5e8_9
SYSDATA_SRCS += $(PKG_ROOT)/data-raw/who/wfa-girls-zscore-expanded-tables.xlsx?sfvrsn=f01bc813_10
SYSDATA_SRCS += $(PKG_ROOT)/data-raw/who/wfh-boys-percentiles-expanded-tables.xlsx?sfvrsn=407ceb43_7
SYSDATA_SRCS += $(PKG_ROOT)/data-raw/who/wfh-boys-zscore-expanded-tables.xlsx?sfvrsn=ac60cb13_8
SYSDATA_SRCS += $(PKG_ROOT)/data-raw/who/wfh-girls-percentiles-expanded-tables.xlsx?sfvrsn=eb27f3ad_7
SYSDATA_SRCS += $(PKG_ROOT)/data-raw/who/wfh-girls-zscore-expanded-tables.xlsx?sfvrsn=daac732c_8
SYSDATA_SRCS += $(PKG_ROOT)/data-raw/who/wfl-boys-percentiles-expanded-tables.xlsx?sfvrsn=41c436e1_7
SYSDATA_SRCS += $(PKG_ROOT)/data-raw/who/wfl-boys-zscore-expanded-table.xlsx?sfvrsn=d307434f_8
SYSDATA_SRCS += $(PKG_ROOT)/data-raw/who/wfl-girls-percentiles-expanded-tables.xlsx?sfvrsn=e50b7713_7
SYSDATA_SRCS += $(PKG_ROOT)/data-raw/who/wfl-girls-zscore-expanded-table.xlsx?sfvrsn=db7b5d6b_8

################################################################################
# Recipes

.PHONY: all check install clean

all: $(PKG_NAME)_$(PKG_VERSION).tar.gz

test:
	${assert}

$(PKG_NAME)_$(PKG_VERSION).tar.gz: .install_dev_deps.Rout .document.Rout $(VIGNETTES) $(TESTS) $(DATATARGETS) $(SHINYAPPS)
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

$(PKG_ROOT)/data/lo2013.rda : data-raw/lo2013.R data-raw/lo2013_bp_weight_height_bmi.txt
	R CMD BATCH --vanilla $<

$(PKG_ROOT)/data/gemelli1990.rda : data-raw/gemelli1990.R data-raw/gemelli1990_female.csv data-raw/gemelli1990_male.csv
	R CMD BATCH --vanilla $<

$(PKG_ROOT)/data/bp_parameters.rda : data-raw/gaussian_parameters.R R/est_norm.R data/gemelli1990.rda  data/nhlbi_bp_norms.rda data/lo2013.rda
	R CMD BATCH --vanilla $<

$(PKG_ROOT)/R/sysdata.rda : data-raw/sysdata.R $(SYSDATA_SRCS)
	R CMD BATCH --vanilla $<

################################################################################
# Other Recipes for checking the package, (un)installing, and cleaning the
# working directory.
#
covr-report-tests.html : $(PKG_NAME)_$(PKG_VERSION).tar.gz
	R --vanilla --quiet -e 'x <- covr::package_coverage(type = c("tests")'\
		-e 'covr::report(x, file = "covr-report-tests.html")'

covr-report-vignettes.html : $(PKG_NAME)_$(PKG_VERSION).tar.gz
	R --vanilla --quiet -e 'x <- covr::package_coverage(type = c("vignettes")'\
		-e 'covr::report(x, file = "covr-report-vignettes.html")'

covr-report-examples.html : $(PKG_NAME)_$(PKG_VERSION).tar.gz
	R --vanilla --quiet -e 'x <- covr::package_coverage(type = c("examples"))'\
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
	$(RM) -f  $(PKG_NAME)_$(PKG_VERSION).tar.gz
	$(RM) -rf $(PKG_NAME).Rcheck
	$(RM) -f .*.Rout
	$(RM) -f *.Rout
	$(RM) -f *.html

