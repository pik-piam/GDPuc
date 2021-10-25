
<!-- README.md is generated from README.Rmd. Please edit that file -->

# GDPuc

<!-- badges: start -->

[![R-CMD-check](https://github.com/johanneskoch94/GDPuc/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/johanneskoch94/GDPuc/actions/workflows/R-CMD-check.yaml)
[![codecov](https://codecov.io/gh/johanneskoch94/GDPuc/branch/main/graph/badge.svg?token=3GHXFQXARX)](https://app.codecov.io/gh/johanneskoch94/GDPuc)
![Lifecycle:
stable](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)

<!-- badges: end -->

GDPuc (a.k.a. the GDP unit-converter) provides a simple function to
convert GDP time-series data from one unit to another. All common GDP
units are supported, i.e.:

-   current LCU
-   current US$MER
-   current Int$PPP
-   constant YYYY LCU
-   constant YYYY US$MER
-   constant YYYY Int$PPP

Here “YYYY” is a placeholder for a year, e.g. “2010” or “2015”, and
“LCU” stands for Local Currency Unit.

## Installation

There currently only exists the development version on github.

Open a R session on your machine and use the `remotes` package to
install GDPuc.

``` r
# Install remotes if necessary:
install.packages("remotes")

# Install GDPuc from github:
remotes::install_github("johanneskoch94/GDPuc")
```

## Usage

Load the package.

``` r
library(GDPuc)
```

The main function of the package is `convertGDP`.

``` r
convertGDP(
  gdp = gdp, 
  unit_in = "constant 2005 LCU", 
  unit_out = "constant 2017 Int$PPP"
)
```

Here, the `gdp` argument takes a tibble or a data-frame that contains,
at least:

-   a column with iso3c country codes, (ideally named “iso3c”)
-   a column with the year, (ideally named “year”)
-   a column named “value”, with the gdp data

A corresponding magpie object, see the
[magclass](https://github.com/pik-piam/magclass) package, is also
accepted.

#### source

Use the `source` argument to control the source of the underlying
conversion factors (GDP deflators, MERs and PPPs). There are two source
options shipped with the package, `wb_wdi` and `wb_wdi_linked` (with
`wb_wdi` set as the default), but custom sources are possible. A custom
source can be any tibble with columns:

-   “iso3c” (character),
-   “year” (numeric),
-   “GDP deflator” (numeric),
-   “MER (LCU per US$)” (numeric),
-   “PPP conversion factor, GDP (LCU per international $)” (numeric)

``` r
convertGDP(
  gdp = gdp, 
  unit_in = "constant 2005 LCU", 
  unit_out = "constant 2017 Int$PPP",
  source = "wb_wdi"
)
```

#### verbose

Set the `verbose` argument to `TRUE` to print out the underlying
conversion steps and factors. The verbosity can also be controlled via
the option `"GDPuc.verbose"`.

``` r
convertGDP(
  gdp = gdp, 
  unit_in = "constant 2005 LCU", 
  unit_out = "constant 2017 Int$PPP",
  verbose = TRUE
)
```

## Further arguments

#### with\_regions

Use the `with_regions` argument to convert aggregated GDP data,
e.g. regional-level data. The default value is `NULL`, but if passed a
data-frame with a country-to-region mapping, then custom regional codons
will be recognized. The data-frame should have two columns, one named
“iso3c” with iso3c country codes, and one named “region” with the
corresponding region codes. The conversion of regional values is then
undertaken by disaggregating the regions to a country level (using the
mapping and weighed by the GDP shares of countries within that region in
the base year of `unit_in`).

``` r
convertGDP(
  gdp = gdp, 
  unit_in = "constant 2005 LCU", 
  unit_out = "constant 2017 Int$PPP",
  with_regions = my_mapping_data_frame
)
```

#### replace\_NAs

Use the `replace_NAs` argument to replace missing conversion factors;
either with 1 (so essentially no conversion) or with regional
GDP-weighted averages. The default value is `NULL`, and missing
conversion factors will lead to NAs. If set to `1`, then no conversion
will take place, but no NAs will be created either. To use regional
GDP-weighted averages, set the argument to “regional\_averages”.

``` r
convertGDP(
  gdp = gdp, 
  unit_in = "constant 2005 LCU", 
  unit_out = "constant 2017 Int$PPP",
  replace_NAs = 1
)

convertGDP(
  gdp = gdp, 
  unit_in = "constant 2005 LCU", 
  unit_out = "constant 2017 Int$PPP",
  with_regions = my_mapping_data_frame,
  replace_NAs = "regional_averages"
)
```

## Looking at the available sources

Use `print_source_info` to print information on a specific, or all
available sources.

``` r
print_source_info("wb_wdi")
print_source_info()
```

Use the `:::` operator to take a closer look at the sources shipped with
GDPuc.

``` r
GDPuc:::wb_wdi
```

## Examples

``` r
library(GDPuc)

gdp <- tibble::tibble(
  iso3c = "USA", 
  year = 2010:2014, 
  value = 100:104
)
print(gdp)
#> # A tibble: 5 × 3
#>   iso3c  year value
#>   <chr> <int> <int>
#> 1 USA    2010   100
#> 2 USA    2011   101
#> 3 USA    2012   102
#> 4 USA    2013   103
#> 5 USA    2014   104

convertGDP(
  gdp = gdp, 
  unit_in = "constant 2005 LCU", 
  unit_out = "constant 2017 Int$PPP"
)
#> # A tibble: 5 × 3
#>   iso3c  year value
#>   <chr> <int> <dbl>
#> 1 USA    2010  123.
#> 2 USA    2011  125.
#> 3 USA    2012  126.
#> 4 USA    2013  127.
#> 5 USA    2014  128.
```

Setting `verbose = TRUE` will additionally print the following
information to the console:

``` r
convertGDP(
  gdp = gdp, 
  unit_in = "constant 2005 LCU", 
  unit_out = "constant 2017 Int$PPP",
  verbose = TRUE
)
#> ℹ Converting GDP with conversion factors from wb_wdi:
#> constant 2005 LCU → constant 2017 LCU
#> 2017 value of base 2005 GDP deflator in (constant 2017 LCU per constant 2005
#> LCU) used:
#> USA: 1.23304244543521
#> constant 2017 LCU → constant 2017 Int$PPP
#> 2017 PPP conversion factor in (LCU per international $) used:
#> USA: 1
#> # A tibble: 5 × 3
#>   iso3c  year value
#>   <chr> <int> <dbl>
#> 1 USA    2010  123.
#> 2 USA    2011  125.
#> 3 USA    2012  126.
#> 4 USA    2013  127.
#> 5 USA    2014  128.
```

Print out information on the underlying `wb_wdi` source.

``` r
print_source_info("wb_wdi")
#> ── wb_wdi ──────────────────────────────────────────────────────────────────────
#> → Origin: The World Bank's World Development Indicator Database
#> → Date: Downloaded on the 6 of May 2021
#> → Html: https://databank.worldbank.org/source/world-development-indicators
#> → Note: Uses the standard deflator.
#> ────────────────────────────────────────────────────────────────────────────────
```

## Conversion method

This package makes us of country-specific GDP deflators, Market Exchange
Rates (MER), and Purchasing Power Parity (PPP) conversion factors to
convert GDP values. Setting the `verbose` argument to `TRUE` should make
the conversion process transparent and allow you to analyze the
individual steps taken. All conversion functions were successfully
tested on the World Bank’s World Development Indicator (WDI) data: given
any 2 WDI GDP series, `convertGDP` will reliably convert the one to the
other.

That being said, converting GDP series can be complex and the use of
this package should not absolve one of thinking carefully on what
conversion is being done and how. When using the provided `wd_wdi`
source, this specifically concerns conversions using PPPs and MERs
together.  
That is because the PPP conversion factors provided by the World Bank
are based off of linked GDP deflators, while the MERs are not. That
means, that converting the World Bank’s current international dollar PPP
series into LCU, will result in the “GDP: linked series (current LCU)”,
while converting the current US dollar MER series into LCU, will result
in the “GDP (current LCU)” series. Therefore, when linked and non-lined
GDP deflators differ (which they do more often for developing countries,
and in general the further into the past one looks), converting from
current IntPPP to current USMER will not result in the exact same series
as given in the WDI data.
