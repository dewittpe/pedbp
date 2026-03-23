# Stature for Age - Pediatric Growth Standard

Stature for age quantile, distribution, and zscore function based on LMS
data from the CDC and WHO.

## Usage

``` r
p_height_for_age(
  q,
  male,
  age,
  source = getOption("pedbp_pgs_source", "CDC"),
  ...
)

q_height_for_age(
  p,
  male,
  age,
  source = getOption("pedbp_pgs_source", "CDC"),
  ...
)

z_height_for_age(
  q,
  male,
  age,
  source = getOption("pedbp_pgs_source", "CDC"),
  ...
)

p_length_for_age(
  q,
  male,
  age,
  source = getOption("pedbp_pgs_source", "CDC"),
  ...
)

q_length_for_age(
  p,
  male,
  age,
  source = getOption("pedbp_pgs_source", "CDC"),
  ...
)

z_length_for_age(
  q,
  male,
  age,
  source = getOption("pedbp_pgs_source", "CDC"),
  ...
)
```

## Arguments

- q:

  a vector of quantiles

- male:

  integer value, 1 = male, 0 = female

- age:

  numeric age, in months

- source:

  a character string denoting the data source providing the parameters
  needed for the estimate. Valid values are "CDC" and "WHO". This can be
  set explicitly, or by using the `pedbp_pgs_source` option.

- ...:

  pass through

- p:

  a vector of probabilities

## Value

`p_` method return values from the estimated distribution function.

`q_` methods return values from the estimated quantile function.

`z_` methods return standard scores.

## Notes

CDC Recommends using WHO growth charts for infants and children ages 0
to 2 years of age in the U.S. and CDC growth charts to monitor growth
for children age 2 years and older in the U.S.

## References

<https://www.cdc.gov/growthcharts/percentile_data_files.htm>,
<https://www.who.int/tools/child-growth-standards/standards>

## See also

- Distribution functions:

  - [`bmi_for_age`](http://www.peteredewitt.com/pedbp/reference/bmi_for_age.md)

  - [`head_circumference_for_age`](http://www.peteredewitt.com/pedbp/reference/head_circumference_for_age.md)

  - `height_for_age`

  - `length_for_age`

  - [`weight_for_age`](http://www.peteredewitt.com/pedbp/reference/weight_for_age.md)

  - [`weight_for_length`](http://www.peteredewitt.com/pedbp/reference/weight_for_stature.md)

  - [`weight_for_height`](http://www.peteredewitt.com/pedbp/reference/weight_for_stature.md)

- Plotting functions:

  - [`gs_chart`](http://www.peteredewitt.com/pedbp/reference/gs_chart.md)

  - [`gs_cdf`](http://www.peteredewitt.com/pedbp/reference/gs_cdf.md)

- Vignette:

  - [`vignette(topic = "growth-standards", package = "pedbp")`](http://www.peteredewitt.com/pedbp/articles/growth-standards.md)

## Examples

``` r
# Find the first quartile height for a 66 month old female.
# The quantile based on CDC data is slightly less than the quantile based on
# the data from the WHO
q_height_for_age(p = 0.25, age = 66, male = 0, source = c("CDC", "WHO"))
#> [1] 107.6194 108.8470

# The 90th quantile length/height for a 24 month female: note that these
# values are similar, but not identical
q_length_for_age(p = 0.9, age = 24, male = 0, source = c("CDC"))
#> [1] 89.79282
q_height_for_age(p = 0.9, age = 24, male = 0, source = c("CDC"))
#> [1] 89.40951

# Find the percentile for a 28 month old male with a stature (height/length)
# of 88 cm
p_height_for_age(q = 88, male = 1, age = 28, source = "CDC")
#> [1] 0.3730417
p_height_for_age(q = 88, male = 1, age = 28, source = "WHO")
#> Warning: age/stature below lower limit
#> [1] NaN
p_length_for_age(q = 88, male = 1, age = 28, source = "CDC")
#> [1] 0.2944436
p_length_for_age(q = 88, male = 1, age = 28, source = "WHO")
#> [1] 0.2325074

# correseponding standard scores
z_height_for_age(q = 88, male = 1, age = 28, source = "CDC")
#> [1] -0.323808
z_height_for_age(q = 88, male = 1, age = 28, source = "WHO")
#> Warning: age/stature below lower limit
#> [1] NaN
z_length_for_age(q = 88, male = 1, age = 28, source = "CDC")
#> [1] -0.5404492
z_length_for_age(q = 88, male = 1, age = 28, source = "WHO")
#> [1] -0.7306143
```
