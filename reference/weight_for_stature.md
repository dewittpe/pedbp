# Weight for Stature - Pediatric Growth Standard

Weight for stature quantile, distribution, and zscore function based on
LMS data from the CDC and WHO.

## Usage

``` r
p_weight_for_height(
  q,
  male,
  height,
  source = getOption("pedbp_pgs_source", "CDC"),
  ...
)

q_weight_for_height(
  p,
  male,
  height,
  source = getOption("pedbp_pgs_source", "CDC"),
  ...
)

z_weight_for_height(
  q,
  male,
  height,
  source = getOption("pedbp_pgs_source", "CDC"),
  ...
)

p_weight_for_length(
  q,
  male,
  length,
  source = getOption("pedbp_pgs_source", "CDC"),
  ...
)

q_weight_for_length(
  p,
  male,
  length,
  source = getOption("pedbp_pgs_source", "CDC"),
  ...
)

z_weight_for_length(
  q,
  male,
  length,
  source = getOption("pedbp_pgs_source", "CDC"),
  ...
)
```

## Arguments

- q:

  a vector of quantiles

- male:

  integer value, 1 = male, 0 = female

- source:

  a character string denoting the data source providing the parameters
  needed for the estimate. Valid values are "CDC" and "WHO". This can be
  set explicitly, or by using the `pedbp_pgs_source` option.

- ...:

  pass through

- p:

  a vector of probabilities

- length, height:

  in centimeters

## Value

`p_` method return values from the estimated distribution function.

`q_` methods return values from the estimated quantile function.

`z_` methods return standard scores.

## Details

Length or height values are used. Length is assess when the patient is
lying down versus height when the patient is standing. There is an
implication of younger patients being in the \_for_length set. There is
some overlap in numeric values of length and height.

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

  - [`height_for_age`](http://www.peteredewitt.com/pedbp/reference/stature_for_age.md)

  - [`length_for_age`](http://www.peteredewitt.com/pedbp/reference/stature_for_age.md)

  - [`weight_for_age`](http://www.peteredewitt.com/pedbp/reference/weight_for_age.md)

  - `weight_for_length`

  - `weight_for_height`

- Plotting functions:

  - [`gs_chart`](http://www.peteredewitt.com/pedbp/reference/gs_chart.md)

  - [`gs_cdf`](http://www.peteredewitt.com/pedbp/reference/gs_cdf.md)

- Vignette:

  - [`vignette(topic = "growth-standards", package = "pedbp")`](http://www.peteredewitt.com/pedbp/articles/growth-standards.md)

## Examples

``` r
# The 60th weight qualtile for a 1.2 meter tall male is
q_weight_for_height(p = 0.60, male = 1, height = 120, source = "CDC")
#> [1] 22.4941
q_weight_for_height(p = 0.60, male = 1, height = 120, source = "WHO")
#> [1] 22.89542

# There are slight differences in the quantiles for length and height
q_weight_for_length(p = 0.60, male = 1, length = 97, source = "CDC")
#> [1] 14.88168
q_weight_for_height(p = 0.60, male = 1, height = 97, source = "CDC")
#> [1] 15.08691

# percentiles and standard scores for a 14 kg, 88 cm tall/long male
p_weight_for_height(q = 14, male = 1, height = 88, source = "CDC")
#> [1] 0.9003879
p_weight_for_height(q = 14, male = 1, height = 88, source = "WHO")
#> [1] 0.9285045
p_weight_for_length(q = 14, male = 1, length = 88, source = "CDC")
#> [1] 0.9277451
p_weight_for_length(q = 14, male = 1, length = 88, source = "WHO")
#> [1] 0.9479553

# correseponding standard scores
z_weight_for_height(q = 14, male = 1, height = 88, source = "CDC")
#> [1] 1.283765
z_weight_for_height(q = 14, male = 1, height = 88, source = "WHO")
#> [1] 1.464743
z_weight_for_length(q = 14, male = 1, length = 88, source = "CDC")
#> [1] 1.459201
z_weight_for_length(q = 14, male = 1, length = 88, source = "WHO")
#> [1] 1.625343

```
