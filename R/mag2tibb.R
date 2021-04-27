##' @title mag2tibb
#' @description   Converts magclass object into tibble with colnames  required for GDPuc
#'
#' @param gdp A magclass object with gdp or gdppc data
#' @importFrom magclass as.data.frame
#' @return A tibble
#' @export
#'

mag2tibb <- function(gdp) {

  # test if regions are iso3 if (getRegions(a))

  #check if there is years info
  if(is.null(getYears(gdp))){
    stop("No year information in mag object!")
  }

gdp <- as_tibble(magclass::as.data.frame(gdp))
colnames(gdp) <- tolower(colnames(gdp))
colnames(gdp)[colnames(gdp) == "region"] <- "iso3c"
gdp$year <- as.integer(as.character(gdp$year))

return(gdp)}



