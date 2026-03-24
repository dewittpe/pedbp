# Data Sets Informing Blood Pressure Percentile Estimates

A collection of data sets from multiple sources used to inform blood
pressure percentiles for pediatrics patients by sex, age, and height (if
known).

## Usage

``` r
lo2013

gemelli1990

nhlbi_bp_norms

flynn2017

bp_parameters
```

## Format

An object of class `data.frame` with 30 rows and 6 columns.

An object of class `data.frame` with 8 rows and 6 columns.

An object of class `data.frame` with 952 rows and 6 columns.

An object of class `data.frame` with 714 rows and 6 columns.

An object of class `data.frame` with 514 rows and 8 columns.

## Details

Data sets are named to reflect the source.

For all the data sets provided units are uniform:

- age::

  Patient age; months

- height::

  length/height/stature; cm

- weight::

  kilograms

- male::

  integer value; 1 = male, 0 = female

- sbp::

  systolic blood pressure; mmHg

- dbp::

  diastolic blood pressure; mmHg

Columns with a name such as `sbp` are point observations. Summary
statistics are appended to the variable as needed, e.g., `sbp_mean` and
`sbp_sd` for the reported mean and standard deviation of systolic blood
pressure.

CDC ages represent whole month but reported at the half month. That is,
age = 12.5 is short-hand for 12 \<= age \< 13. The exception is birth;
age = 0 is birth and not a range.

`bp_parameters` has the estimated mean and standard deviations for
estimating percentiles using a Gaussian distribution for a given sex,
age (in months), and height (if known/applicable).

## References

Expert Panel on Integrated Guidelines for Cardiovascular Health and Risk
Reduction in Children and Adolescents. "Expert panel on integrated
guidelines for cardiovascular health and risk reduction in children and
adolescents: summary report." Pediatrics 128.Supplement_5 (2011):
S213-S256.

Gemelli, M., Manganaro, R., Mami, C., & De Luca, F. (1990). Longitudinal
study of blood pressure during the 1st year of life. European journal of
pediatrics, 149(5), 318-320.

Lo, Joan C., et al. "Prehypertension and hypertension in community-based
pediatric practice." Pediatrics 131.2 (2013): e415-e424.

The Fourth Report on the Diagnosis, Evaluation, and Treatment of High
Blood Pressure in Children and Adolescents National High Blood Pressure
Education Program Working Group on High Blood Pressure in Children and
Adolescents Pediatrics 2004;114;555-576
\<doi:10.1542/peds.114.2.S2.555\>

Flynn, Joseph T., David C. Kaelber, Carissa M. Baker-Smith, Douglas
Blowey, Aaron E. Carroll, Stephen R. Daniels, Sarah D. De Ferranti et
al. "Clinical practice guideline for screening and management of high
blood pressure in children and adolescents." Pediatrics 140, no. 3
(2017).

## See also

[`vignette("bp-distributions", package = "pedbp")`](http://www.peteredewitt.com/pedbp/articles/bp-distributions.md)
