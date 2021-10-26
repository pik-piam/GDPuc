smart_select_iso3c <- function(gdp) {
  gdp %>%
    dplyr::select(tidyselect::vars_select_helpers$where(
      ~ (is.character(.x) || is.factor(.x)) &&
        all(nchar(as.character(.x)) == 3) &&
        all(.x == toupper(.x))
    )) %>%
    colnames()
}

smart_select_year <- function(gdp) {
  gdp %>%
    dplyr::select(tidyselect::vars_select_helpers$where(
      ~ is.numeric(.x) &&
        all(!is.na(.x)) &&
        all(nchar(as.character(.x)) == 4)
    )) %>%
    colnames()
}

mag_2_tibb <- function(gdp) {
  gdp %>%
    magclass::as.data.frame() %>%
    tibble::as_tibble() %>%
    dplyr::select(-.data$Cell) %>%
    dplyr::rename("iso3c" = "Region", "year" = "Year", "value" = "Value") %>%
    dplyr::mutate(year = as.integer(as.character(.data$year)))
}