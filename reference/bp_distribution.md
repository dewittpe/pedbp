# Pediatric Blood Pressure Distribution

Distribution and quantile functions for pediatric blood pressure.

## Usage

``` r
p_bp(
  q_sbp,
  q_dbp,
  age,
  male,
  height = NA,
  height_percentile = NA,
  default_height_percentile = 50,
  source = getOption("pedbp_bp_source", "martin2022"),
  ...
)

q_bp(
  p_sbp,
  p_dbp,
  age,
  male,
  height = NA,
  height_percentile = NA,
  default_height_percentile = 50,
  source = getOption("pedbp_bp_source", "martin2022"),
  ...
)

z_bp(
  q_sbp,
  q_dbp,
  age,
  male,
  height = NA,
  height_percentile = NA,
  default_height_percentile = 50,
  source = getOption("pedbp_bp_source", "martin2022"),
  ...
)
```

## Arguments

- q_sbp:

  a vector of systolic blood pressures

- q_dbp:

  a vector of diastolic blood pressures

- age:

  numeric age, in months

- male:

  integer value, 1 = male, 0 = female

- height:

  numeric, in centimeters, can be missing. See Details.

- height_percentile:

  height percentile to use; range from \[0, 100\]. See Details.

- default_height_percentile:

  default height percentile to use if `height` is missing; range (0,
  100).

- source:

  the method, or data set, to use as the reference. See Details.

- ...:

  not currently used

- p_sbp:

  a vector of systolic blood probabilities; range from \[0, 1\].

- p_dbp:

  a vector of diastolic blood probabilities; range from \[0, 1\].

## Value

A `pedbp_bp` object. This is a list of two numeric vectors for the
systolic and diastolic pressure respectively. The names for the vectors
depends on the call. `p_bp` returns a list of vectors with the names:
`sbp_p` and `dbp_p`. `q_bp` returns a list of vectors with names: `sbp`
and `dbp`. `z_bp` returns a list of vectors with names: `sbp_z` and
`dbp_z`.

Additionally, a `pedbp_bp` object has a `bp_params` attribute which
provides details on the data source and parameters used in the
estimates.

## Details

`source` is used to specify the method or source data sets by which the
distributions are estimated. This can be controlled by the option
`pedbp_bp_source`. End users are encouraged to set the option if not
using the default so all calls to these functions will use the same
source.

Options:

- `martin2022` (default) uses a combination of references to generate
  distribution values for ages 1 months through 18 years, without or
  without known stature. This was the only method implemented in version
  1 of the pedbp package.

- `gemelli1990` uses only the reference values from Gemelli et al.
  (1990). These values are applicable to patients from 1 month to 12
  months of age. Stature is not used in the look up for the parameters.

- `lo2013` uses only the reference values from Lo et al. (2013). This is
  applicable to patients of at least three years of age. Height is not
  considered when looking up the parameters.

- `nhlbi` uses only reference values from the National Heart, Lung, and
  Blood Institute \[NHLBI\] and the Centers for Disease Control and
  Prevention \[CDC\] published in 2011. These are for patients of at
  least one year of age and with a known stature. These values were
  publish

- `flynn2017` uses only reference values from Flynn et al. (2017). These
  values are similar to the nhlbi values \_but\_ "do not include
  children and adolescents with overweight and obesity (ie, those with a
  BMI \>= 85th percentile).

There is a hierarchy for the use of the `height`, `height_percentile`,
and `default_height_percentile`. If `height` is provided, it takes
precedence over the other two arguments. `height_percentile` is used if
`height` is missing and takes precedence over
`default_height_percentile`. The height is only needed if using the
`nhlbi` or `flynn2017` data sources (including as part of the
`martin2022` workflow).

## References

Gemelli, Marina, Rosa Manganaro, Carmelo Mamì, and F. De Luca.
"Longitudinal study of blood pressure during the 1st year of life."
European journal of pediatrics 149 (1990): 318-320.

Lo, Joan C., Alan Sinaiko, Malini Chandra, Matthew F. Daley, Louise C.
Greenspan, Emily D. Parker, Elyse O. Kharbanda et al. "Prehypertension
and hypertension in community-based pediatric practice." Pediatrics 131,
no. 2 (2013): e415-e424.

"Expert panel on integrated guidelines for cardiovascular health and
risk reduction in children and adolescents: summary report." Pediatrics
128, no. Suppl 5 (2011): S213. \<doi:10.1542/peds.2009-2107C\>

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

[`vignette("bp-distributions", package = "pedbp")`](http://www.peteredewitt.com/pedbp/articles/bp-distributions.md),
[`bp_cdf`](http://www.peteredewitt.com/pedbp/reference/bp_cdf.md) for
plotting cumulative distribution functions for the blood pressures.

## Examples

``` r
x <- p_bp(q_sbp = 100, q_dbp = 60, age = 8, male = 0)
x
#> $sbp_p
#> [1] 0.7881446
#> 
#> $dbp_p
#> [1] 0.6554217
#> 
str(x)
#> List of 2
#>  $ sbp_p: num 0.788
#>  $ dbp_p: num 0.655
#>  - attr(*, "bp_params")='data.frame':    1 obs. of  8 variables:
#>   ..$ source           : chr "gemelli1990"
#>   ..$ male             : int 0
#>   ..$ age              : num 6
#>   ..$ sbp_mean         : num 92
#>   ..$ sbp_sd           : num 10
#>   ..$ dbp_mean         : num 56
#>   ..$ dbp_sd           : num 10
#>   ..$ height_percentile: num NA
#>  - attr(*, "class")= chr [1:2] "pedbp_bp" "pedbp_p_bp"

x <- p_bp(q_sbp = c(NA, 82), q_dbp = c(60, 72), age = 9.2, male = 0)
x
#> $sbp_p
#> [1]        NA 0.1150697
#> 
#> $dbp_p
#> [1] 0.6461698 0.9696036
#> 
str(x)
#> List of 2
#>  $ sbp_p: num [1:2] NA 0.115
#>  $ dbp_p: num [1:2] 0.646 0.97
#>  - attr(*, "bp_params")='data.frame':    2 obs. of  8 variables:
#>   ..$ source           : chr [1:2] "gemelli1990" "gemelli1990"
#>   ..$ male             : int [1:2] 0 0
#>   ..$ age              : num [1:2] 9 9
#>   ..$ sbp_mean         : num [1:2] 94 94
#>   ..$ sbp_sd           : num [1:2] 10 10
#>   ..$ dbp_mean         : num [1:2] 57 57
#>   ..$ dbp_sd           : num [1:2] 8 8
#>   ..$ height_percentile: num [1:2] NA NA
#>  - attr(*, "class")= chr [1:2] "pedbp_bp" "pedbp_p_bp"

x <- p_bp(q_sbp = c(NA, 82), q_dbp = c(60, 72), age = 29.2, male = 0, height = 82.8)
x
#> $sbp_p
#> [1]        NA 0.3847185
#> 
#> $dbp_p
#> [1] 0.9398825 0.9959809
#> 
str(x)
#> List of 2
#>  $ sbp_p: num [1:2] NA 0.385
#>  $ dbp_p: num [1:2] 0.94 0.996
#>  - attr(*, "bp_params")='data.frame':    2 obs. of  8 variables:
#>   ..$ source           : chr [1:2] "nhlbi" "nhlbi"
#>   ..$ male             : int [1:2] 0 0
#>   ..$ age              : num [1:2] 24 24
#>   ..$ sbp_mean         : num [1:2] 85 85
#>   ..$ sbp_sd           : num [1:2] 10.2 10.2
#>   ..$ dbp_mean         : num [1:2] 43 43
#>   ..$ dbp_sd           : num [1:2] 10.9 10.9
#>   ..$ height_percentile: num [1:2] 5 5
#>  - attr(*, "class")= chr [1:2] "pedbp_bp" "pedbp_p_bp"

x <- q_bp(p_sbp = 0.78, p_dbp = 0.65, age = 8, male = 0)
x
#> $sbp
#> [1] 99.72193
#> 
#> $dbp
#> [1] 59.8532
#> 
str(x)
#> List of 2
#>  $ sbp: num 99.7
#>  $ dbp: num 59.9
#>  - attr(*, "bp_params")='data.frame':    1 obs. of  8 variables:
#>   ..$ source           : chr "gemelli1990"
#>   ..$ male             : int 0
#>   ..$ age              : num 6
#>   ..$ sbp_mean         : num 92
#>   ..$ sbp_sd           : num 10
#>   ..$ dbp_mean         : num 56
#>   ..$ dbp_sd           : num 10
#>   ..$ height_percentile: num NA
#>  - attr(*, "class")= chr [1:2] "pedbp_bp" "pedbp_q_bp"

#############################################################################
# compare results when height is known or unknown
p_bp(q_sbp = rep(100, 2),
     q_dbp = rep( 60, 2),
     age   = rep(35.75, 2),
     male  = c(0, 0),
     height = c(NA, 100))
#> $sbp_p
#> [1] 0.8799462 0.8108822
#> 
#> $dbp_p
#> [1] 0.9151897 0.8858366
#> 

#############################################################################
# Working with multiple patients records
d <- read.csv(system.file("example_data", "for_batch.csv", package = "pedbp"))
d
#>           pid age_months male height..cm. sbp..mmHg. dbp..mmHg.
#> 1   patient_A         96    1          NA        102         58
#> 2   patient_B        144    0         153        113         NA
#> 3   patient_C          4    0          62         82         43
#> 4 patient_D_1         41    1          NA         96         62
#> 5 patient_D_2         41    1         101         96         62

bp_percentiles <-
  p_bp(
      q_sbp = d$sbp..mmHg.
    , q_dbp = d$dbp..mmHg.
    , age   = d$age_months
    , male  = d$male
    )
bp_percentiles
#> $sbp_p
#> [1] 0.5533069 0.7320575 0.2622697 0.6195685 0.6195685
#> 
#> $dbp_p
#> [1] 0.4120704        NA 0.1356661 0.8028518 0.8028518
#> 

# Standard (z) scores:
z_bp(
    q_sbp = d$sbp..mmHg.
  , q_dbp = d$dbp..mmHg.
  , age   = d$age_months
  , male  = d$male
  )
#> $sbp_z
#> [1]  0.1340206  0.6190476 -0.6363636  0.3043478  0.3043478
#> 
#> $dbp_z
#> [1] -0.2222222         NA -1.1000000  0.8518519  0.8518519
#> 

q_bp(
    p_sbp = bp_percentiles$sbp_p
  , p_dbp = bp_percentiles$dbp_p
  , age   = d$age_months
  , male  = d$male
  )
#> $sbp
#> [1] 102 113  82  96  96
#> 
#> $dbp
#> [1] 58 NA 43 62 62
#> 


#############################################################################
# Selecting different source values

# default
p_bp(q_sbp = 92, q_dbp = 60, age = 29.2, male = 0, default_height_percentile = 0.95,
     source = "martin2022")
#> $sbp_p
#> [1] 0.7534673
#> 
#> $dbp_p
#> [1] 0.9398825
#> 
p_bp(q_sbp = 92, q_dbp = 60, age = 29.2, male = 0, default_height_percentile = 0.95,
     source = "gemelli1990")
#> $sbp_p
#> [1] NA
#> 
#> $dbp_p
#> [1] NA
#> 
p_bp(q_sbp = 92, q_dbp = 60, age = 29.2, male = 0, default_height_percentile = 0.95,
     source = "lo2013")
#> $sbp_p
#> [1] NA
#> 
#> $dbp_p
#> [1] NA
#> 
p_bp(q_sbp = 92, q_dbp = 60, age = 29.2, male = 0, default_height_percentile = 0.95,
     source = "nhlbi")
#> $sbp_p
#> [1] 0.7534673
#> 
#> $dbp_p
#> [1] 0.9398825
#> 
p_bp(q_sbp = 92, q_dbp = 60, age = 29.2, male = 0, default_height_percentile = 0.95,
     source = "flynn2017")
#> $sbp_p
#> [1] 0.6797595
#> 
#> $dbp_p
#> [1] 0.9290757
#> 

q_bp(p_sbp = 0.85, p_dbp = 0.85, age = 29.2, male = 0, default_height_percentile = 0.95,
     source = "martin2022") # default
#> $sbp
#> [1] 95.58685
#> 
#> $dbp
#> [1] 54.33901
#> 
q_bp(p_sbp = 0.85, p_dbp = 0.85, age = 29.2, male = 0, default_height_percentile = 0.95,
     source = "gemelli1990")
#> $sbp
#> [1] NA
#> 
#> $dbp
#> [1] NA
#> 
q_bp(p_sbp = 0.85, p_dbp = 0.85, age = 29.2, male = 0, default_height_percentile = 0.95,
     source = "lo2013")
#> $sbp
#> [1] NA
#> 
#> $dbp
#> [1] NA
#> 
q_bp(p_sbp = 0.85, p_dbp = 0.85, age = 29.2, male = 0, default_height_percentile = 0.95,
     source = "nhlbi")
#> $sbp
#> [1] 95.58685
#> 
#> $dbp
#> [1] 54.33901
#> 
q_bp(p_sbp = 0.85, p_dbp = 0.85, age = 29.2, male = 0, default_height_percentile = 0.95,
     source = "flynn2017")
#> $sbp
#> [1] 98.08443
#> 
#> $dbp
#> [1] 55.5823
#> 

```
