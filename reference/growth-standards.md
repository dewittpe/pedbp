# Pediatric Growth Standard Distribution Functions

Growth standards based on data from the Centers for Disease Control and
the World Health Organization.

## Arguments

- q:

  a vector of quantiles

- p:

  a vector of probabilities on the 0 to 1 scale

- male:

  integer value, 1 = male, 0 = female

- age:

  numeric age, in months

- length, height:

  in centimeters

- source:

  a character string denoting the data source providing the parameters
  needed for the estimate. Valid values are "CDC" and "WHO". This can be
  set explicitly, or by using the `pedbp_pgs_source` option.

- ...:

  pass through

## Value

`p_` method return values from the estimated distribution function.

`q_` methods return values from the estimated quantile function.

`z_` methods return standard scores.

## Notes

CDC Recommends using WHO growth charts for infants and children ages 0
to 2 years of age in the U.S. and CDC growth charts to monitor growth
for children age 2 years and older in the U.S.

## Notation

Arguments named `p` denote probabilities on the 0 to 1 scale. When
percentiles are discussed in text, tables, or figures, they are
expressed as percentile points on the 0 to 100 scale.

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

  - [`weight_for_length`](http://www.peteredewitt.com/pedbp/reference/weight_for_stature.md)

  - [`weight_for_height`](http://www.peteredewitt.com/pedbp/reference/weight_for_stature.md)

- Plotting functions:

  - [`gs_chart`](http://www.peteredewitt.com/pedbp/reference/gs_chart.md)

  - [`gs_cdf`](http://www.peteredewitt.com/pedbp/reference/gs_cdf.md)

- Vignette:

  - [`vignette(topic = "growth-standards", package = "pedbp")`](http://www.peteredewitt.com/pedbp/articles/growth-standards.md)
