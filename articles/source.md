# Choosing conversion factors

## The `source` argument

Use the `source` argument in `convertGDP` to control the source of the
underlying conversion factors (GDP deflators, MERs and PPPs). This can
be one of the sources shipped with the package or a user-defined object.

### Package internal ‘sources’

There are two `source` options shipped with the package, `wb_wdi` and
`wb_wdi_linked`, **with `wb_wdi` set as the default**. Pass the name of
a shipped source to the source argument to use it.

``` r
library(GDPuc)

my_gdp <- tibble::tibble(
  iso3c = "USA", 
  year = 2010:2014, 
  value = 100:104
)

convertGDP(
  gdp = my_gdp, 
  unit_in = "constant 2010 LCU", 
  unit_out = "constant 2014 Int$PPP",
  source = "wb_wdi_linked",
  verbose = TRUE
)
#> ℹ Converting GDP with conversion factors from wb_wdi_linked:
#> constant 2010 LCU → constant 2014 LCU
#> 2014 value of base 2010 GDP deflator in (constant 2014 LCU per constant 2010
#> LCU) used:
#> USA: 1.07786
#> constant 2014 LCU → constant 2014 Int$PPP
#> 2014 PPP conversion factor in (LCU per international $) used:
#> USA: 1
#> # A tibble: 5 × 3
#>   iso3c  year value
#>   <chr> <int> <dbl>
#> 1 USA    2010  108.
#> 2 USA    2011  109.
#> 3 USA    2012  110.
#> 4 USA    2013  111.
#> 5 USA    2014  112.
```

Use the function `print_source_info` to print information on a specific,
or all available sources.

``` r
print_source_info("wb_wdi")
#> ── wb_wdi ──────────────────────────────────────────────────────────────────────
#> → Origin: The World Bank's World Development Indicator Database
#> → Date: Downloaded on the 30th of April 2024
#> → Html: https://databank.worldbank.org/source/world-development-indicators
#> → Note: Uses the GDP deflator.
#> ────────────────────────────────────────────────────────────────────────────────
print_source_info()
#> ℹ Sources available:
#> ── wb_wdi ──────────────────────────────────────────────────────────────────────
#> → Origin: The World Bank's World Development Indicator Database
#> → Date: Downloaded on the 30th of April 2024
#> → Html: https://databank.worldbank.org/source/world-development-indicators
#> → Note: Uses the GDP deflator.
#> ────────────────────────────────────────────────────────────────────────────────
#> ── wb_wdi_linked ───────────────────────────────────────────────────────────────
#> → Origin: The World Bank's World Development Indicator Database
#> → Date: Downloaded on the 30th of April 2024
#> → Html: https://databank.worldbank.org/source/world-development-indicators
#> → Note: Uses the linked GDP deflator.
#> ────────────────────────────────────────────────────────────────────────────────
#> ── wb_wdi_cpi ──────────────────────────────────────────────────────────────────
#> → Origin: The World Bank's World Development Indicator Database
#> → Date: Downloaded on the 30th of April 2024
#> → Html: https://databank.worldbank.org/source/world-development-indicators
#> → Note: Uses the CPI as deflator.
#> ────────────────────────────────────────────────────────────────────────────────
```

Use the `:::` operator to take a closer look at sources shipped with
GDPuc.

``` r
GDPuc:::wb_wdi
```

### User-defined ‘source’ objects

Any tibble with columns:

- “iso3c” (character),
- “year” (numeric),
- “GDP deflator” (numeric),
- “MER (LCU per US\$)” (numeric),
- “PPP conversion factor, GDP (LCU per international \$)” (numeric)

can be used as a source of conversion factors.

``` r
my_custom_source <- tibble::tibble(
  iso3c = "USA", 
  year = 2010:2014, 
  "GDP deflator" = seq(1, 1.1, 0.025),
  "MER (LCU per US$)" = 1,
  "PPP conversion factor, GDP (LCU per international $)" = 1,
)
print(my_custom_source)
#> # A tibble: 5 × 5
#>   iso3c  year `GDP deflator` `MER (LCU per US$)` PPP conversion factor, GDP (L…¹
#>   <chr> <int>          <dbl>               <dbl>                           <dbl>
#> 1 USA    2010           1                      1                               1
#> 2 USA    2011           1.02                   1                               1
#> 3 USA    2012           1.05                   1                               1
#> 4 USA    2013           1.08                   1                               1
#> 5 USA    2014           1.1                    1                               1
#> # ℹ abbreviated name: ¹​`PPP conversion factor, GDP (LCU per international $)`

convertGDP(
  gdp = my_gdp, 
  unit_in = "constant 2010 LCU", 
  unit_out = "constant 2014 Int$PPP",
  source = my_custom_source,
  verbose = TRUE
)
#> ℹ Converting GDP with conversion factors from user_provided:
#> constant 2010 LCU → constant 2014 LCU
#> 2014 value of base 2010 GDP deflator in (constant 2014 LCU per constant 2010
#> LCU) used:
#> USA: 1.1
#> constant 2014 LCU → constant 2014 Int$PPP
#> 2014 PPP conversion factor in (LCU per international $) used:
#> USA: 1
#> # A tibble: 5 × 3
#>   iso3c  year value
#>   <chr> <int> <dbl>
#> 1 USA    2010  110 
#> 2 USA    2011  111.
#> 3 USA    2012  112.
#> 4 USA    2013  113.
#> 5 USA    2014  114.
```

## The `use_USA_cf_for_all` argument

In some cases it may be desirable to use the US conversion factors for
all countries. For instance, when converting global scenario data from
modelling efforts, in US\$MER, to another base year. Setting the
`use_USA_cf_for_all` to `TRUE` ensures that all countries are converted
with the US conversion factors.

``` r
my_gdp <- tibble::tibble(
  iso3c = c("USA", "IND"), 
  value = 100
)

# Normal conversion, with country specific conversion factors
convertGDP(
  gdp = my_gdp, 
  unit_in = "constant 2005 US$MER", 
  unit_out = "constant 2020 US$MER",
  verbose = TRUE
)
#> ℹ Converting GDP with conversion factors from wb_wdi:
#> constant 2005 US$MER → constant 2005 LCU
#> 2005 MERs in (LCU per US$) used:
#> IND: 44.2736
#> USA: 1
#> constant 2005 LCU → constant 2020 LCU
#> 2020 value of base 2005 GDP deflators in (constant 2020 LCU per constant 2005
#> LCU) used:
#> IND: 2.35925
#> USA: 1.30033
#> constant 2020 LCU → constant 2020 US$MER
#> 2020 MERs in (LCU per US$) used:
#> IND: 74.225
#> USA: 1
#> # A tibble: 2 × 2
#>   iso3c value
#>   <chr> <dbl>
#> 1 USA    130.
#> 2 IND    141.

# Using the US conversion factors for both countries
convertGDP(
  gdp = my_gdp, 
  unit_in = "constant 2005 US$MER", 
  unit_out = "constant 2020 US$MER",
  use_USA_cf_for_all = TRUE,
  verbose = TRUE
)
#> ℹ Converting GDP with conversion factors from wb_wdi:
#> constant 2005 US$MER → constant 2005 LCU
#> 2005 MERs in (LCU per US$) used:
#> IND: 1
#> USA: 1
#> constant 2005 LCU → constant 2020 LCU
#> 2020 value of base 2005 GDP deflators in (constant 2020 LCU per constant 2005
#> LCU) used:
#> IND: 1.30033
#> USA: 1.30033
#> constant 2020 LCU → constant 2020 US$MER
#> 2020 MERs in (LCU per US$) used:
#> IND: 1
#> USA: 1
#> # A tibble: 2 × 2
#>   iso3c value
#>   <chr> <dbl>
#> 1 USA    130.
#> 2 IND    130.
```
