
<!-- README.md is generated from README.Rmd. Please edit that file -->

# GDPuc

<!-- badges: start -->

[![R-CMD-check](https://github.com/johanneskoch94/GDPuc/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/johanneskoch94/GDPuc/actions/workflows/R-CMD-check.yaml)

<!-- badges: end -->

GDPuc (a.k.a. GDP unit-conversion) provides a single function to convert
GDP time-series data from one unit to another. All common GDP units are
supported.

## Installation

Since this is still a private repository, you’ll need a [github personal
acces
token](https://docs.github.com/en/github/authenticating-to-github/creating-a-personal-access-token)
with “repo” scope. Once you’ve created this token, you can use it to
install GDPuc directly from github.

Open a R session on your machine, either in Rstudio (recommended), or
from the command line with the `R` command.

``` r
# Make sure devtools is installed:
install.packages("devtools")

# Install GDPuc from github:
devtools::install_github(
  repo = "johanneskoch94/GDPuc",
  auth_token = "<<your personal github acces token goes here>>"
)
```

## Usage

The function `convertGDP` takes a tibble that contains at least:

-   a column with iso3c country codes, (ideally named “iso3c”)
-   a column with the year, (ideally named “year”)
-   a column named “value”, with the gdp data

and converts the “value” column from the unit specified by the `unit_in`
argument, into that specified by `unit_out`. It uses GDP deflators, MERs
and PPPs from the database specified by the `source` argument (defaults
to the world bank’s Feb. 2021 WDI).

Allowed in- and out-units are:

-   “current LCU”
-   “current US$MER”
-   “current Int$PPP”
-   “constant YYYY LCU”
-   “constant YYYY US$MER”
-   “constant YYYY Int$PPP”

where “YYYY” should be replaced by a year, e.g. “2010” or “2015”.

``` r
# Load package into R-session
library(GDPuc)

# Convert gdp
convertGDP(
  gdp = gdp, 
  unit_in = "constant 2005 LCU", 
  unit_out = "constant 2017 Int$PPP"
)
```

## Example

``` r
library(GDPuc)

gdp <- tibble::tibble(iso3c = "USA", year = 2010:2020, value = 100:110)
print(gdp)
#> # A tibble: 11 x 3
#>    iso3c  year value
#>    <chr> <int> <int>
#>  1 USA    2010   100
#>  2 USA    2011   101
#>  3 USA    2012   102
#>  4 USA    2013   103
#>  5 USA    2014   104
#>  6 USA    2015   105
#>  7 USA    2016   106
#>  8 USA    2017   107
#>  9 USA    2018   108
#> 10 USA    2019   109
#> 11 USA    2020   110

convertGDP(gdp, "constant 2005 LCU", "constant 2017 Int$PPP")
#> # A tibble: 11 x 3
#>    iso3c  year value
#>    <chr> <int> <dbl>
#>  1 USA    2010  123.
#>  2 USA    2011  124.
#>  3 USA    2012  126.
#>  4 USA    2013  127.
#>  5 USA    2014  128.
#>  6 USA    2015  129.
#>  7 USA    2016  131.
#>  8 USA    2017  132.
#>  9 USA    2018  133.
#> 10 USA    2019  134.
#> 11 USA    2020  136.
```
