#' @title mag_2_tibb
#' @description   Converts magclass object into tibble with colnames required
#'   for convertGDP.
#'
#' @param gdp A magclass object with gdp or gdppc data
#' @return A tibble
mag_2_tibb <- function(gdp) {
  gdp %>%
    magclass::as.data.frame() %>%
    tibble::as_tibble() %>%
    dplyr::select(-.data$Cell) %>%
    dplyr::rename("iso3c" = "Region", "year" = "Year", "value" = "Value") %>%
    dplyr::mutate(year = as.integer(as.character(.data$year)))
}
