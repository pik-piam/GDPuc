### Re-submission after email from Victoria Wimmer from the 17th of July

* Package description was amended following the recommendations (doesn't start with "function to").
* Missing "value" section in documentation of a function was added (the recommended "No return value, called for side effects" was used.)
* As I understand it, the package was previously archived because of the warning "Does not use Suggests conditionally". This issue was addressed, by making sure that the packages listed under "suggests" are now used conditionally in the package tests and vignettes. I do not get any warnings anymore when checking, only the "checking CRAN incoming feasibility" note, indicating that the package was archived. 

### R CMD check results

0 errors | 0 warnings | 0 notes

