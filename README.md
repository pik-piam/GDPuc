
<!-- README.md is generated from README.Rmd. Please edit that file -->

# GDPuc

<!-- badges: start -->

[![R-CMD-check](https://github.com/johanneskoch94/GDPuc/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/johanneskoch94/GDPuc/actions/workflows/R-CMD-check.yaml)
[![Codecov test
coverage](https://codecov.io/gh/johanneskoch94/GDPuc/branch/main/graph/badge.svg)](https://codecov.io/gh/johanneskoch94/GDPuc?branch=main)
<!-- badges: end -->

GDPuc provides a single function to convert GDP time-series data from
one unit to another. All common GDP units are supported.

## Installation

## Example

``` r
library(GDPuc)

convertGDP(gdp, "constant 2005 Int$PPP", "constant 2017 Int$PPP")
```
