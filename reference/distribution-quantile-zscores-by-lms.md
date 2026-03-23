# Distribution, Quantile, and Z-scores by LMS values

Functions for getting estimated distribution, quantile, and standard
scores (z-scores) given LMS parameters.

## Usage

``` r
zlms(x, l, m, s, ...)

plms(x, l, m, s, ...)

qlms(x, l, m, s, ...)
```

## Arguments

- x:

  quantile or probability value

- l, m, s:

  the lms values

- ...:

  pass through

## Value

a numeric vector

## Details

The parameters need to be either length 1 or of equal length.

L is the power in the Box-Cox transformation, M the median, and S a
generalized coefficient of variation. For a given standard score
(z-score), Z, the value X of interest is

\$\$ X = \begin{cases} M (1 + LSZ)^{1/L} & L \neq 0 \\ M \exp(SZ) & L =
0. \end{cases} \$\$

To get the z-score for a value X:

\$\$Z = \begin{cases} \frac{ \left(\frac{X}{M}\right)^{L} - 1 }{LS} & L
\neq 0 \\ \frac{\log\left(\frac{X}{M}\right)}{S} & L = 0.
\end{cases}\$\$

## References

Cole, Timothy J., and Pamela J. Green. "Smoothing reference centile
curves: the LMS method and penalized likelihood." Statistics in medicine
11.10 (1992): 1305-1319.

## Examples

``` r
l <- -0.1600954
m <-  9.476500305
s <-  0.11218624

# the 5th quantile:
qlms(x = 0.05, l = l, m = m, s = s)
#> [1] 7.900755

# What percentile is the value 8.2?
plms(x = 8.2, l = l, m = m, s = s)
#> [1] 0.09599727

# What is the standard score for the value 8.2
zlms(x = 8.2, l = l, m = m, s = s)
#> [1] -1.304701

all.equal(
  zlms(x = 8.2, l = l, m = m, s = s)
  ,
  qnorm(plms(x = 8.2, l = l, m = m, s = s))
)
#> [1] TRUE

# get all the quantiles form the 5th through 95th for a set of LMS parameters
ps <- seq(0.05, 0.95, by = 0.05)
qs <- qlms(x = ps, l = l, m = m, s = s)
all.equal(plms(qs, l, m, s), ps)
#> [1] TRUE
all.equal(zlms(x = qs, l = l, m = m, s = s), qnorm(ps))
#> [1] TRUE
```
