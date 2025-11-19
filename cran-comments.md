## Resubmission

This is the second attempt at submitting this version, after the CRAN automatic checks found an issue when checking the "noSuggests" case. The issues were resolved by adding skip_if_not_installed lines in the corresponding tests and vignettes.

## Minor update to the package

* Fixes #30, #32, #36
* Fix bug where countries without PPP data were not getting the US conversion factors.
* Fix dissagregtion of regions to ignore with missing conversion factors
* Make package compatible with madrat caching
* Add option to pass madrat regionmapping string to with_regions
* Drop dependency on "lifecycle" package.
* Improve documentation and add message when verbose is TRUE
* Introduce xCU as unit (local CU of any country x)
* Improve documentation
* Add arguments to specify non-default iso3c and year columns

## R CMD check results

0 errors | 0 warnings | 0 notes


