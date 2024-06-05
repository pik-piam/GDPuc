# Base year of constant US$MER GDP series in wdi data
year_USMER <- 2015
regex_var_USMER <- paste("GDP \\(constant", year_USMER, "US\\$\\)")
var_USMER <- paste("GDP (constant", year_USMER, "US$)")

# Base year of constant Int$PPP GDP series in wb_wdi
year_IntPPP <- 2017
regex_var_IntPPP <- paste("GDP, PPP \\(constant", year_IntPPP, "international \\$\\)")
var_IntPPP <- paste("GDP, PPP (constant", year_IntPPP, "international $)")

# The WDI is not always consistent. In January 2024 only PAN was causing problems. In April 2024 a couple of
# additional countries are not consistent in every year. These countries are removed from the tests.
bad_countries <- c("PAN", "SWE", "NOR", "JPN", "CZE", "FIN", "CAN")

# Countries with the euro as currency
euro_countries <- c("AUT", "BEL", "HRV", "EST", "FIN", "FRA", "DEU", "GRC", "IRL",
                    "ITA", "LVA", "LTU", "LUX", "NLD", "PRT", "SVK", "SVN", "ESP")
