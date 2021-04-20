
<!-- README.md is generated from README.Rmd. Please edit that file -->

# GDPuc

<!-- badges: start -->

[![R-CMD-check](https://github.com/johanneskoch94/GDPuc/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/johanneskoch94/GDPuc/actions/workflows/R-CMD-check.yaml)

<!-- badges: end -->

GDPuc (a.k.a. GDP unit-conversion) provides a single function to convert
GDP time-series data from one unit to another. All common GDP units are
supported, i.e.:

-   current LCU
-   current US$MER
-   current Int$PPP
-   constant YYYY LCU
-   constant YYYY US$MER
-   constant YYYY Int$PPP

where “YYYY” is a placeholder for a year, e.g. “2010” or “2015”.

## Installation

Since this is still a private repository, you’ll need a [github personal
acces
token](https://docs.github.com/en/github/authenticating-to-github/creating-a-personal-access-token)
with “repo” scope. Once you’ve created this token, you can use it to
install GDPuc directly from github.

Open a R session on your machine, either in Rstudio (recommended), or
from the command line with the `R` command.

``` r
# Make sure remotes is installed:
install.packages("remotes")

# Install GDPuc from github:
remotes::install_github(
  repo = "johanneskoch94/GDPuc",
  auth_token = "<<your personal github acces token goes here>>"
)
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

Here, the `gdp` argument takes a tibble that contains, at least:

-   a column with iso3c country codes, (ideally named “iso3c”)
-   a column with the year, (ideally named “year”)
-   a column named “value”, with the gdp data

Use the `source` argument to control the source of the underlying
conversion factors (GDP deflators, MERs and PPPs). There are 2 source
options shipped with the package, with `wb_wdi` set as default, but
custom sources are possible.

``` r
convertGDP(
  gdp = gdp, 
  unit_in = "constant 2005 LCU", 
  unit_out = "constant 2017 Int$PPP",
  source = "imf_weo"
)
```

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

## Example

``` r
library(GDPuc)

gdp <- tibble::tibble(
  iso3c = "USA", 
  year = 2010:2014, 
  value = 100:104
)
print(gdp)
#> # A tibble: 5 x 3
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
#> # A tibble: 5 x 3
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
```

<img src="man/figures/README-/example2.svg" width="100%" />
