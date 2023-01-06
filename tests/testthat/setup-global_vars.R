# Base year of constant US$MER GDP series in wdi data
year_USMER <- 2010
regex_var_USMER <- paste("GDP \\(constant", year_USMER, "US\\$\\)")

# Base year of constant Int$PPP GDP series in wb_wdi
year_IntPPP <- 2017
regex_var_IntPPP <- paste("GDP, PPP \\(constant", year_IntPPP, "international \\$\\)")

# WSM = Samoa has inconsistent WDI data...
bad_countries <- c("WSM")
