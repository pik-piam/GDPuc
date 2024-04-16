# Base year of constant US$MER GDP series in wdi data
year_USMER <- 2015
regex_var_USMER <- paste("GDP \\(constant", year_USMER, "US\\$\\)")
var_USMER <- paste("GDP (constant", year_USMER, "US$)")

# Base year of constant Int$PPP GDP series in wb_wdi
year_IntPPP <- 2017
regex_var_IntPPP <- paste("GDP, PPP \\(constant", year_IntPPP, "international \\$\\)")
var_IntPPP <- paste("GDP, PPP (constant", year_IntPPP, "international $)")

# PAN = Panama has inconsistent WDI data...
bad_countries <- c("PAN")

# Countries with the euro as currency
euro_countries <- c("AUT", "BEL", "HRV", "EST", "FIN", "FRA", "DEU", "GRC", "IRL",
                    "ITA", "LVA", "LTU", "LUX", "NLD", "PRT", "SVK", "SVN", "ESP")
