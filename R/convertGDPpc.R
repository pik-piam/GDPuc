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
convertGDPpc <- function(gdppc, unit_in, unit_out, source = "wb_wdi", pop_source = source, verbose = FALSE) {

  pop <- eval(rlang::sym(pop_source)) %>%
    dplyr::select(iso3c, year, `Population, total`)

  return_mag <- FALSE
  if (is.magpie(gdppc)){
    return_mag=TRUE
    gdppc <- mag2tibb(gdppc)
    }

  gdp <- gdppc %>%
    dplyr::left_join(pop, by = c("iso3c", "year")) %>%
    dplyr::mutate(value = value * `Population, total`, .keep = "unused")

  x <- convertGDP(gdp, unit_in, unit_out, source, verbose)

  x <- x %>%
    dplyr::left_join(pop, by = c("iso3c", "year")) %>%
    dplyr::mutate(value = value / `Population, total`, .keep = "unused")

  if (return_mag){
    x <- as.magpie(x[,-1], spatial="iso3c", temporal="year")
    if(any(is.na(x))){warning("NAs may have been generated for countries lacking conversion factors!")}
  }

  return(x)
}
