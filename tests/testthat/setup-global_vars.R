# Base year of constant US$MER GDP series in wdi data
year_USMER <- 2015
regex_var_USMER <- paste("GDP \\(constant", year_USMER, "US\\$\\)")
var_USMER <- paste("GDP (constant", year_USMER, "US$)")

# Base year of constant Int$PPP GDP series in wb_wdi
year_IntPPP <- 2021
regex_var_IntPPP <- paste("GDP, PPP \\(constant", year_IntPPP, "international \\$\\)")
var_IntPPP <- paste("GDP, PPP (constant", year_IntPPP, "international $)")

# The WDI is not always consistent, and different countries cause issues in different versions.
# These countries are removed from the tests.
# In January 2024 only "PAN" was causing problems.
# In April 2024 "PAN", "SWE", "NOR", "JPN", "CZE", "FIN" and "CAN" were causing problems.
# In July 2026 "IND" causes problems.
bad_countries <- c("IND")
