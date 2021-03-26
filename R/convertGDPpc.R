#' Convert GDP per capita
#'
#' @param gdppc A tibble with 3 columns "iso3c", "year" and "value", where value
#'   is gdp per capita.
#' @param pop_source A string indicating the source database to use for
#'   population numbers. Defaults to same source used for inflation and PPP
#'   factors.
#' @inheritParams convertGDP
#'
#' @return A tibble
#' @export
convertGDPpc <- function(gdppc, unit_in, unit_out, source = "imf_weo", pop_source = source, verbose = FALSE) {

  pop <- eval(rlang::sym(pop_source)) %>%
    dplyr::select(iso3c, year, `Population, total`)

  gdp <- gdppc %>%
    dplyr::left_join(pop, by = c("iso3c", "year")) %>%
    dplyr::mutate(value = value * `Population, total`, .keep = "unused")

  x <- convertGDP(gdp, unit_in, unit_out, source, verbose)

  x <- x %>%
    dplyr::left_join(pop, by = c("iso3c", "year")) %>%
    dplyr::mutate(value = value / `Population, total`, .keep = "unused")

  return(x)
}
