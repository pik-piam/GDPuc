
<!-- README.md is generated from README.Rmd. Please edit that file -->

# GDPuc

<!-- badges: start -->

[![CRAN
status](https://www.r-pkg.org/badges/version/GDPuc)](https://CRAN.R-project.org/package=GDPuc)
[![R-CMD-check](https://github.com/johanneskoch94/GDPuc/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/johanneskoch94/GDPuc/actions/workflows/R-CMD-check.yaml)
[![codecov](https://codecov.io/gh/johanneskoch94/GDPuc/branch/main/graph/badge.svg?token=3GHXFQXARX)](https://app.codecov.io/gh/johanneskoch94/GDPuc)
![Lifecycle:
stable](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)

<!-- badges: end -->

GDPuc (a.k.a. the GDP unit-converter) provides a simple function to
convert GDP time-series data from one unit to another.

**To note:** The default conversion parameters are from the World Bank’s
World Development Indicators (WDI) database (see
[link](https://databank.worldbank.org/source/world-development-indicators)).
The current parameters are from **October 2021**, with the next update
planned for October 2022.

## Installation

``` r
# Install from CRAN
install.packages("GDPuc")

# Or the development version from GitHub
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
  gdp = my_gdp, 
  unit_in = "constant 2005 LCU", 
  unit_out = "constant 2017 Int$PPP"
)
```

Here, the `gdp` argument takes a tibble or a data-frame that contains,
at least:

-   a column with iso3c country codes, (ideally named “iso3c”),
-   a column with the year, (ideally named “year”),
-   a column named “value”, with the gdp data.

The `unit_in` and `unit_out` arguments specify the incoming and outgoing
GDP units. All common GDP units are supported, i.e.:

-   current LCU
-   current US$MER
-   current Int$PPP
-   constant YYYY LCU
-   constant YYYY US$MER
-   constant YYYY Int$PPP

Here “YYYY” is a placeholder for a year, e.g. “2010” or “2015”, and
“LCU” stands for Local Currency Unit.

## Example

``` r
library(GDPuc)

my_gdp <- tibble::tibble(
  iso3c = "USA", 
  year = 2010:2014, 
  value = 100:104
)
print(my_gdp)
#> # A tibble: 5 × 3
#>   iso3c  year value
#>   <chr> <int> <int>
#> 1 USA    2010   100
#> 2 USA    2011   101
#> 3 USA    2012   102
#> 4 USA    2013   103
#> 5 USA    2014   104

convertGDP(
  gdp = my_gdp, 
  unit_in = "constant 2005 LCU", 
  unit_out = "constant 2017 Int$PPP"
)
#> # A tibble: 5 × 3
#>   iso3c  year value
#>   <chr> <int> <dbl>
#> 1 USA    2010  123.
#> 2 USA    2011  124.
#> 3 USA    2012  126.
#> 4 USA    2013  127.
#> 5 USA    2014  128.
```

## Further Options

`convertGDP` has other arguments that allow you to:

-   choose conversion factors (see [“Choosing conversion
    factors”](https://johanneskoch94.github.io/GDPuc/articles/source.html))

-   print out information on the conversion process and/or return the
    conversion factors used (see [“Getting information on the conversion
    process”](https://johanneskoch94.github.io/GDPuc/articles/verbose.html))

-   handle missing conversion factors (see [“Handling missing conversion
    factors”](https://johanneskoch94.github.io/GDPuc/articles/handle_NAs.html))

-   convert regional GDP data (see [“Converting regional GDP
    data”](https://johanneskoch94.github.io/GDPuc/articles/with_regions.html))
