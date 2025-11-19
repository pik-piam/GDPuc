# Converting regional GDP data

## The `with_regions` argument

Use the `with_regions` argument (default `NULL`) in `convertGDP` to
convert aggregated GDP data, e.g. regional-level data.

### `with_regions =` a data-frame with a country-to-region mapping

If passed a data-frame with a country-to-region mapping, then custom
regional codons will be recognized. The data-frame should have two
columns, one named “iso3c” with iso3c country codes, and one named
“region” with the corresponding region codes. The conversion of regional
values is then undertaken by disaggregating the regions to a country
level using the mapping, and weighed by the GDP shares of countries
within that region in the base year of `unit_in` (to compute the shares,
the source object needs to have GDP data for the countries within the
region).

``` r
library(GDPuc)

my_gdp <- tibble::tibble(
  iso3c = "EUR", 
  year = 2010:2014, 
  value = 100:104
)

my_mapping_data_frame <- tibble::tibble(
  iso3c = c("DEU", "FRA", "ESP", "ITA"), 
  region = "EUR"
)

convertGDP(
  gdp = my_gdp, 
  unit_in = "constant 2005 Int$PPP", 
  unit_out = "constant 2017 Int$PPP",
  with_regions = my_mapping_data_frame,
  verbose = TRUE
)
#> ℹ Dissaggreagting regions using GDP in constant 2005 Int$PPP as weights.
#> ℹ Converting GDP with conversion factors from wb_wdi:
#> constant 2005 Int$PPP → constant 2005 LCU
#> 2005 PPP conversion factors in (LCU per international $) used:
#> DEU: 0.872721
#> ESP: 0.769508
#> FRA: 0.916458
#> ITA: 0.855139
#> constant 2005 LCU → constant 2017 LCU
#> 2017 value of base 2005 GDP deflators in (constant 2017 LCU per constant 2005
#> LCU) used:
#> DEU: 1.17967
#> ESP: 1.1273
#> FRA: 1.14739
#> ITA: 1.18511
#> constant 2017 LCU → constant 2017 Int$PPP
#> 2017 PPP conversion factors in (LCU per international $) used:
#> DEU: 0.744783
#> ESP: 0.630839
#> FRA: 0.770109
#> ITA: 0.689895
#> # A tibble: 5 × 3
#>   iso3c  year value
#>   <chr> <int> <dbl>
#> 1 EUR    2010  140.
#> 2 EUR    2011  141.
#> 3 EUR    2012  142.
#> 4 EUR    2013  144.
#> 5 EUR    2014  145.
```

### `with_regions =` a string with a madrat regionmapping

If passed a string, then a corresponding regionmapping will be loaded
with
[`madrat::toolGetMapping`](https://rdrr.io/pkg/madrat/man/toolGetMapping.html).
Requires madrat to be installed, and the regionmapping to exist.

``` r
my_gdp <- tibble::tibble(
  iso3c = "EUR", 
  value = 100
)

convertGDP(
  gdp = my_gdp, 
  unit_in = "constant 2005 Int$PPP", 
  unit_out = "constant 2017 Int$PPP",
  with_regions = "regionmappingH12.csv"
)
#> # A tibble: 1 × 2
#>   iso3c value
#>   <chr> <dbl>
#> 1 EUR    138.
```
