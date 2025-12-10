# Changelog

## GDPuc 1.6.1

- Fix bug in selection of iso3c column

## GDPuc 1.6.0

CRAN release: 2025-11-19

- Fixes [\#30](https://github.com/pik-piam/GDPuc/issues/30),
  [\#32](https://github.com/pik-piam/GDPuc/issues/32),
  [\#36](https://github.com/pik-piam/GDPuc/issues/36)
- Fix bug where countries without PPP data were not getting the US
  conversion factors.
- Fix dissagregtion of regions to ignore with missing conversion factors
- Make package compatible with madrat caching
- Add option to pass madrat regionmapping string to with_regions
- Drop dependency on “lifecycle” package.
- Improve documentation and add message when verbose is TRUE
- Introduce xCU as unit (local CU of any country x)
- Improve documentation
- Add arguments to specify non-default iso3c and year columns

## GDPuc 1.0.4

CRAN release: 2024-09-04

- Improve internal column detection
- Allow magclass objects without years

## GDPuc 1.0.0

CRAN release: 2024-06-05

- Update wdi conversion factors and data
- Add the CPI as possible deflator
- Add two shortcut functions for using the CPI as deflator and convert a
  single value (fixed
  [\#20](https://github.com/pik-piam/GDPuc/issues/20))
- Add example to convertGDP (fixes
  [\#21](https://github.com/pik-piam/GDPuc/issues/21))
- Add options to convert to constant € and use only US deflator
- Add with_USA option to replace_NAs
- Relax restriction on presence of a year column (fixes
  [\#17](https://github.com/pik-piam/GDPuc/issues/17))
- Improve order of tests
- Use magclass::as_tibble() instead of custom function

## GDPuc 0.11.1

CRAN release: 2023-07-20

- Clean up Suggest field, in order to comply with CRAN specifications
- Improve documentation

## GDPuc 0.11.0

CRAN release: 2023-06-19

- Fix issue [\#15](https://github.com/pik-piam/GDPuc/issues/15). The cfs
  data-frame returned when “return_cfs = TRUE” now displays the correct
  conversion factors.
- Get rid of dplyr warnings and improve tests

## GDPuc 0.10.0

CRAN release: 2023-01-05

- Get rid of tidyselect warnings and improve tests

## GDPuc 0.9.2

CRAN release: 2022-09-19

- Transfer base repo to the pik-piam organization and update
  documentation links accordingly.

## GDPuc 0.9.0

CRAN release: 2022-04-07

- Add option GDPuc.warn and corresponding vignette.

- Add `replace_NAs` argument option NA, to silence warning about NA
  creation.

## GDPuc 0.8.0

CRAN release: 2022-02-15

- Add options “no_conversion” for the `replace_NAs` argument.

- The `replace_NAs` argument can now take a vector allowing for a
  combination of options.

## GDPuc 0.7.0

CRAN release: 2022-02-08

- Add argument `return_cfs` to return the conversion factors used.

- Add options “linear” and “linear_regional_average” for the
  `replace_NAs` argument.

- Add vignettes

- Remove dependency on the stringr package

- Improve tests

## GDPuc 0.5.1

CRAN release: 2021-10-26

First public release.
