# Print information on sources

Print detailed information on conversion factor sources to the screen.
Information includes the name, origin, date, html-link and an associated
note. Calling the function without any argument will print information
on all available sources.

## Usage

``` r
print_source_info(source)
```

## Arguments

- source:

  Empty, or the name of one of the internal sources:

  1.  "wb_wdi"

  2.  "wb_wdi_linked"

  3.  "wb_wdi_cpi"

## Value

No return value, called for side effects.

## Examples

``` r
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
