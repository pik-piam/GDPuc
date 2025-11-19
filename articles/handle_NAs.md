# Handling missing conversion factors

## The `replace_NAs` argument

Use the `replace_NAs` argument in `convertGDP` to handle missing
conversion factors.

### `replace_NAs` = NULL or NA

By default, `replace_NAs` is `NULL`, and NAs are returned along with a
warning. Set `replace_NAs = NA` to explicitly return NAs without the
warning.

Below, the `return_cfs` argument is set to `TRUE` to inspect the
conversion factors, along side the result.

``` r
library(GDPuc)

# Test with Venezuela -> iso3c = VEN
my_gdp <- tibble::tibble(
  iso3c = c("VEN"),
  year = 2010:2014,
  value = 100:104
)

x <- convertGDP(
  gdp = my_gdp,
  unit_in = "constant 2005 Int$PPP",
  unit_out = "constant 2019 Int$PPP",
  return_cfs = TRUE
)
#> Warning: NAs have been generated for countries lacking conversion factors!
x$result
#> # A tibble: 5 × 3
#>   iso3c  year value
#>   <chr> <int> <dbl>
#> 1 VEN    2010    NA
#> 2 VEN    2011    NA
#> 3 VEN    2012    NA
#> 4 VEN    2013    NA
#> 5 VEN    2014    NA

x$cfs
#> # A tibble: 1 × 4
#>   iso3c \033[34m2005 PPP convers…¹ \033[34m2019 value o…² \033[34m2019 PPP con…³
#>   <chr>                      <dbl>                  <dbl>                  <dbl>
#> 1 VEN                        0.842                     NA                     NA
#> # ℹ abbreviated names:
#> #   ¹​`\033[34m2005 PPP conversion factor\033[39m in (LCU per international $)`,
#> #   ²​`\033[34m2019 value of base 2005 GDP deflator\033[39m in (constant 2019 LCU per constant 2005 LCU)`,
#> #   ³​`\033[34m2019 PPP conversion factor\033[39m in (LCU per international $)`
```

To eliminate the warning:

``` r
x <- convertGDP(
  gdp = my_gdp, 
  unit_in = "constant 2005 Int$PPP", 
  unit_out = "constant 2019 Int$PPP",
  replace_NAs = NA
)
```

You can also use the `GDPuc.warn` option to suppress warnings from
`convertGDP` in general (see [“Silence
warnings”](https://pik-piam.github.io/GDPuc/articles/warn.html)).

### `replace_NAs` = 0

If set to 0, resulting NAs are set to 0.

``` r
my_gdp <- tibble::tibble(
  iso3c = "VEN",
  year = 2010:2014,
  value = 100:104
)

x <- convertGDP(
  gdp = my_gdp,
  unit_in = "constant 2005 Int$PPP",
  unit_out = "constant 2019 Int$PPP",
  replace_NAs = 0,
  return_cfs = TRUE
)
x$result
#> # A tibble: 5 × 3
#>   iso3c  year value
#>   <chr> <int> <dbl>
#> 1 VEN    2010     0
#> 2 VEN    2011     0
#> 3 VEN    2012     0
#> 4 VEN    2013     0
#> 5 VEN    2014     0

x$cfs
#> # A tibble: 1 × 4
#>   iso3c \033[34m2005 PPP convers…¹ \033[34m2019 value o…² \033[34m2019 PPP con…³
#>   <chr>                      <dbl>                  <dbl>                  <dbl>
#> 1 VEN                        0.842                     NA                     NA
#> # ℹ abbreviated names:
#> #   ¹​`\033[34m2005 PPP conversion factor\033[39m in (LCU per international $)`,
#> #   ²​`\033[34m2019 value of base 2005 GDP deflator\033[39m in (constant 2019 LCU per constant 2005 LCU)`,
#> #   ³​`\033[34m2019 PPP conversion factor\033[39m in (LCU per international $)`
```

### `replace_NAs` = “no_conversion”

If set to “no_conversion”, NAs are replaced with the values in the gdp
argument.

``` r
my_gdp <- tibble::tibble(
  iso3c = "VEN",
  year = 2010:2014,
  value = 100:104
)

x <- convertGDP(
  gdp = my_gdp,
  unit_in = "constant 2005 Int$PPP",
  unit_out = "constant 2019 Int$PPP",
  replace_NAs = "no_conversion",
  return_cfs = TRUE
)
x$result
#> # A tibble: 5 × 3
#>   iso3c  year value
#>   <chr> <int> <dbl>
#> 1 VEN    2010   100
#> 2 VEN    2011   101
#> 3 VEN    2012   102
#> 4 VEN    2013   103
#> 5 VEN    2014   104

x$cfs
#> # A tibble: 1 × 4
#>   iso3c \033[34m2005 PPP convers…¹ \033[34m2019 value o…² \033[34m2019 PPP con…³
#>   <chr>                      <dbl>                  <dbl>                  <dbl>
#> 1 VEN                        0.842                     NA                     NA
#> # ℹ abbreviated names:
#> #   ¹​`\033[34m2005 PPP conversion factor\033[39m in (LCU per international $)`,
#> #   ²​`\033[34m2019 value of base 2005 GDP deflator\033[39m in (constant 2019 LCU per constant 2005 LCU)`,
#> #   ³​`\033[34m2019 PPP conversion factor\033[39m in (LCU per international $)`
```

### `replace_NAs` = “linear”

If set to “linear”, missing conversion factors are inter- and
extrapolated linearly. For the extrapolation, the closest 5 data points
are used.

``` r
my_gdp <- tibble::tibble(
  iso3c = "VEN",
  year = 2010:2014,
  value = 100:104
)

x <- convertGDP(
  gdp = my_gdp,
  unit_in = "constant 2005 Int$PPP",
  unit_out = "constant 2019 Int$PPP",
  replace_NAs = "linear",
  return_cfs = TRUE
)
x$result
#> # A tibble: 5 × 3
#>   iso3c  year value
#>   <chr> <int> <dbl>
#> 1 VEN    2010  203.
#> 2 VEN    2011  205.
#> 3 VEN    2012  208.
#> 4 VEN    2013  210.
#> 5 VEN    2014  212.

x$cfs
#> # A tibble: 1 × 4
#>   iso3c \033[34m2005 PPP convers…¹ \033[34m2019 value o…² \033[34m2019 PPP con…³
#>   <chr>                      <dbl>                  <dbl>                  <dbl>
#> 1 VEN                        0.842                   14.4                   5.97
#> # ℹ abbreviated names:
#> #   ¹​`\033[34m2005 PPP conversion factor\033[39m in (LCU per international $)`,
#> #   ²​`\033[34m2019 value of base 2005 GDP deflator\033[39m in (constant 2019 LCU per constant 2005 LCU)`,
#> #   ³​`\033[34m2019 PPP conversion factor\033[39m in (LCU per international $)`
```

### `replace_NAs` = “regional_average”

If set to “regional_average”, the regional GDP-weighted averages will be
used. Requires a region-mapping, and a column in the source object with
GDP data at PPP, to be used as weight. **May lead to misleading results,
use with care!**

``` r
my_gdp <- tibble::tibble(
  iso3c = "VEN",
  year = 2010:2014,
  value = 100:104
)

my_mapping_data_frame <- tibble::tibble(
  iso3c = c("VEN", "BRA", "ARG", "COL"),
  region = "LAM"
)

x <- convertGDP(
  gdp = my_gdp,
  unit_in = "constant 2005 Int$PPP",
  unit_out = "constant 2019 Int$PPP",
  replace_NAs = "regional_average",
  with_regions = my_mapping_data_frame,
  return_cfs = TRUE
)
x$result
#> # A tibble: 5 × 3
#>   iso3c  year value
#>   <chr> <int> <dbl>
#> 1 VEN    2010 0.485
#> 2 VEN    2011 0.489
#> 3 VEN    2012 0.494
#> 4 VEN    2013 0.499
#> 5 VEN    2014 0.504

x$cfs
#> # A tibble: 1 × 3
#>   iso3c \033[34m2019 value of base 2005 GDP deflator\03…¹ \033[34m2019 PPP con…²
#>   <chr>                                             <dbl>                  <dbl>
#> 1 VEN                                                1.18                   205.
#> # ℹ abbreviated names:
#> #   ¹​`\033[34m2019 value of base 2005 GDP deflator\033[39m in (constant 2019 LCU per constant 2005 LCU)`,
#> #   ²​`\033[34m2019 PPP conversion factor\033[39m in (LCU per international $)`

# Compare the 2019 PPP with the 2005 PPP. They are not in the same order of magnitude. 
# Obviously, being a part of the same region, does not mean the currencies are of the same strength.
```

### `replace_NAs` = c(“linear”, “…”)

If a vector is passed, with “linear” as first element, then the
operations are done in sequence. For example for c(“linear”, 0), missing
conversion factors are first inter- and extrapolated linearly but if any
missing conversion factors still lead to NAs, these are replaced with 0.

``` r
# Create an imaginary country XXX, and add it to the Latin America region
my_gdp <- tibble::tibble(
  iso3c = c("VEN", "XXX"),
  year = 2010,
  value = 100
)

my_mapping_data_frame <- tibble::tibble(
  iso3c = c("VEN", "BRA", "ARG", "COL", "XXX"),
  region = "LAM"
)

x <- convertGDP(
  gdp = my_gdp,
  unit_in = "constant 2005 Int$PPP",
  unit_out = "constant 2019 Int$PPP",
  replace_NAs = c("linear", 0),
  with_regions = my_mapping_data_frame,
  return_cfs = TRUE
)
x$result
#> # A tibble: 2 × 3
#>   iso3c  year value
#>   <chr> <dbl> <dbl>
#> 1 VEN    2010  203.
#> 2 XXX    2010    0

x$cfs
#> # A tibble: 2 × 3
#>   iso3c \033[34m2019 value of base 2005 GDP deflator\03…¹ \033[34m2019 PPP con…²
#>   <chr>                                             <dbl>                  <dbl>
#> 1 VEN                                                14.4                   5.97
#> 2 XXX                                                NA                    NA   
#> # ℹ abbreviated names:
#> #   ¹​`\033[34m2019 value of base 2005 GDP deflator\033[39ms in (constant 2019 LCU per constant 2005 LCU)`,
#> #   ²​`\033[34m2019 PPP conversion factor\033[39ms in (LCU per international $)`
```

### `replace_NAs` = “with_USA”

If set to “with_USA”, missing conversion factors are extended using the
growth rates of the USA. If that is not possible (for instance if there
is no data for any years at all) the data for these countries is
converted using the conversion factors of the USA.

``` r
# Venezuela is only missing conversion factors in 2019, AIA has no conversion factors at all.
my_gdp <- tibble::tibble(
  iso3c = c("VEN", "AIA", "USA"),
  value = 100
)

x <- convertGDP(
  gdp = my_gdp,
  unit_in = "constant 2005 Int$PPP",
  unit_out = "constant 2019 Int$PPP",
  replace_NAs = "with_USA",
  return_cfs = TRUE
)
x$result
#> # A tibble: 3 × 2
#>   iso3c value
#>   <chr> <dbl>
#> 1 VEN    264.
#> 2 AIA    128.
#> 3 USA    128.

x$cfs
#> # A tibble: 3 × 4
#>   iso3c \033[34m2005 PPP convers…¹ \033[34m2019 value o…² \033[34m2019 PPP con…³
#>   <chr>                      <dbl>                  <dbl>                  <dbl>
#> 1 USA                        1                       1.28                   1   
#> 2 VEN                        0.842                   8.40                   2.68
#> 3 AIA                        1                       1.28                   1   
#> # ℹ abbreviated names:
#> #   ¹​`\033[34m2005 PPP conversion factor\033[39ms in (LCU per international $)`,
#> #   ²​`\033[34m2019 value of base 2005 GDP deflator\033[39ms in (constant 2019 LCU per constant 2005 LCU)`,
#> #   ³​`\033[34m2019 PPP conversion factor\033[39ms in (LCU per international $)`
```
