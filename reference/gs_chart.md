# Pediatric Growth Standard Charts

Growth standards based on data from the Centers for Disease Control and
the World Health Organization.

## Usage

``` r
gs_chart(
  metric,
  male = 0:1,
  source = getOption("pedbp_pgs_source", "CDC"),
  p = c(0.01, 0.05, 0.1, 0.25, 0.5, 0.75, 0.9, 0.95, 0.99)
)
```

## Arguments

- metric:

  character string, one of the growth standards

- male:

  integer value, 1 = male, 0 = female

- source:

  a character string denoting the data source providing the parameters
  needed for the estimate. Valid values are "CDC" and "WHO". This can be
  set explicitly, or by using the `pedbp_pgs_source` option.

- p:

  a numeric vector of the probabilities, provided in values between 0
  and 1, to plot

## Value

A `ggplot` object

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

  - [`weight_for_length`](http://www.peteredewitt.com/pedbp/reference/weight_for_stature.md)

  - [`weight_for_height`](http://www.peteredewitt.com/pedbp/reference/weight_for_stature.md)

- Plotting functions:

  - `gs_chart`

  - [`gs_cdf`](http://www.peteredewitt.com/pedbp/reference/gs_cdf.md)

- Vignette:

  - [`vignette(topic = "growth-standards", package = "pedbp")`](http://www.peteredewitt.com/pedbp/articles/growth-standards.md)

## Examples

``` r
gs_chart("bmi_for_age", male = 0)

gs_chart("bmi_for_age", male = 1)

gs_chart("bmi_for_age", male = 0:1)


# add a point for a specific patient
pt <- data.frame(p = 0.82, age = 156, bmi = q_bmi_for_age(p = 0.82, male = 1, age = 156))
gs_chart("bmi_for_age", male = 1) +
  ggplot2::geom_point(data = pt, mapping = ggplot2::aes(x = age, y = bmi))


# select specific percentiles to plot
gs_chart("weight_for_height", male = 0:1, p = c(0.10, 0.80))

```
