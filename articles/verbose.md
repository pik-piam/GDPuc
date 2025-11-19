# Getting information on the conversion process

## The `verbose` argument

Set the `verbose` argument in `convertGDP` to `TRUE` to print out the
underlying conversion steps and factors.

``` r
library(GDPuc)

my_gdp <- tibble::tibble(
  iso3c = "USA", 
  year = 2010:2014, 
  value = 100:104
)

convertGDP(
  gdp = my_gdp, 
  unit_in = "constant 2005 LCU", 
  unit_out = "constant 2017 Int$PPP",
  verbose = TRUE
)
#> ℹ Converting GDP with conversion factors from wb_wdi:
#> constant 2005 LCU → constant 2017 LCU
#> 2017 value of base 2005 GDP deflator in (constant 2017 LCU per constant 2005
#> LCU) used:
#> USA: 1.23136
#> constant 2017 LCU → constant 2017 Int$PPP
#> 2017 PPP conversion factor in (LCU per international $) used:
#> USA: 1
#> # A tibble: 5 × 3
#>   iso3c  year value
#>   <chr> <int> <dbl>
#> 1 USA    2010  123.
#> 2 USA    2011  124.
#> 3 USA    2012  126.
#> 4 USA    2013  127.
#> 5 USA    2014  128.
```

The verbosity can also be controlled via the option `GDPuc.verbose`.

``` r
options(GDPuc.verbose = TRUE)

convertGDP(
  gdp = my_gdp,
  unit_in = "constant 2005 LCU",
  unit_out = "constant 2017 Int$PPP"
)
#> ℹ Converting GDP with conversion factors from wb_wdi:
#> constant 2005 LCU → constant 2017 LCU
#> 2017 value of base 2005 GDP deflator in (constant 2017 LCU per constant 2005
#> LCU) used:
#> USA: 1.23136
#> constant 2017 LCU → constant 2017 Int$PPP
#> 2017 PPP conversion factor in (LCU per international $) used:
#> USA: 1
#> # A tibble: 5 × 3
#>   iso3c  year value
#>   <chr> <int> <dbl>
#> 1 USA    2010  123.
#> 2 USA    2011  124.
#> 3 USA    2012  126.
#> 4 USA    2013  127.
#> 5 USA    2014  128.

options(GDPuc.verbose = FALSE)
```

## The `return_cfs` argument

Set the `return_cfs` argument in `convertGDP` to `TRUE` to return a list
of length 2, with the result and a the conversion factors used.

``` r
convertGDP(
  gdp = my_gdp,
  unit_in = "constant 2005 LCU",
  unit_out = "constant 2017 Int$PPP", 
  return_cfs = TRUE
)
#> $result
#> # A tibble: 5 × 3
#>   iso3c  year value
#>   <chr> <int> <dbl>
#> 1 USA    2010  123.
#> 2 USA    2011  124.
#> 3 USA    2012  126.
#> 4 USA    2013  127.
#> 5 USA    2014  128.
#> 
#> $cfs
#> # A tibble: 1 × 3
#>   iso3c \033[34m2017 value of base 2005 GDP deflator\03…¹ \033[34m2017 PPP con…²
#>   <chr>                                             <dbl>                  <dbl>
#> 1 USA                                                1.23                      1
#> # ℹ abbreviated names:
#> #   ¹​`\033[34m2017 value of base 2005 GDP deflator\033[39m in (constant 2017 LCU per constant 2005 LCU)`,
#> #   ²​`\033[34m2017 PPP conversion factor\033[39m in (LCU per international $)`
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
