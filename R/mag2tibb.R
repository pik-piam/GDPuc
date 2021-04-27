#' @title mag2tibb
#' @description   Converts magclass object into tibble with colnames required
#'   for convertGDP.
#'
#' @param gdp A magclass object with gdp or gdppc data
#' @return A tibble
mag2tibb <- function(gdp) {
  gdp <- tibble::as_tibble(magclass::as.data.frame(gdp))
  colnames(gdp) <- tolower(colnames(gdp))
  colnames(gdp)[colnames(gdp) == "region"] <- "iso3c"
  gdp$year <- as.integer(as.character(gdp$year))
  return(gdp)
}
