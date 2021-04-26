#' Make sure that user input is valid
#'
#' check_user_input performs some checks on the function arguments.
#'
#' @inheritParams convertGDP
#'
#' @return TRUE or an Error
check_user_input <- function(gdp, unit_in, unit_out, source, verbose) {

  if (magclass::is.magpie(gdp)){
    gdp <-mag2tibb(gdp)
  }

  # Check 'gdp' input parameter
  if (!tibble::is_tibble(gdp)) {
    rlang::abort("Invalid 'gdp' argument. `gdp` is not a tibble nor a magclass object.")
  }
  if (! "value" %in% colnames(gdp)) {
    rlang::abort("Invalid 'gdp' argument. `gdp` does not have the required 'value' column.")
  }
  if (! "iso3c" %in% colnames(gdp)) {
    rlang::warn("Invalid 'gdp' argument. `gdp` does not have the required 'iso3c' column.")
    i_iso3c <- gdp %>%
      dplyr::select(tidyselect:::where(~is.character(.x) & nchar(.x[[1]]) == 3)) %>%
      colnames()
    if (identical(i_iso3c, character(0))) {
      rlang::abort("Invalid 'gdp' argument. `gdp` does not have the required 'iso3c' column, and no other column could be identified in its stead.")
    }
    rlang::inform(glue::glue("Couldn't find 'iso3c' column in 'gdp'. \\
                             Assuming {i_iso3c} represents iso3c country codes."))
  }
  if (! "year" %in% colnames(gdp)) {
    rlang::warn("Invalid 'gdp' argument. `gdp` does not have the required 'year' column.")
    i_year <- gdp %>%
      dplyr::select(tidyselect:::where(~is.numeric(.x) &
                                         !is.na(.x[[1]]) &
                                         nchar(as.character(.x[[1]])) == 4)) %>%
      colnames()
    if (identical(i_year, character(0))) {
      rlang::abort("Invalid 'gdp' argument. `gdp` does not have the required 'year' column, and no other column could be identified in its stead.")
    }
    rlang::inform(glue::glue("Couldn't find 'year' column in 'gdp'. \\
                             Assuming {i_year} represents years."))
  }


  # Check input parameters 'unit_in' and 'unit_out'
  if (grepl("constant", unit_in)) {
    base_x <- stringr::str_match(unit_in, "constant (....)")[,2] %>% as.double()
    unit_in <- sub(base_x, "YYYY", unit_in)
  }
  if (grepl("constant", unit_out)) {
    base_y <- stringr::str_match(unit_out, "constant (....)")[,2] %>% as.double()
    unit_out <- sub(base_y, "YYYY", unit_out)
  }
  valid_units <- c(
    "current LCU",
    "current US$MER",
    "current Int$PPP",
    "constant YYYY LCU",
    "constant YYYY US$MER",
    "constant YYYY Int$PPP"
  )
  if (!unit_in %in% valid_units) {
    rlang::abort("Invalid 'unit_in' argument.")
  }
  if (!unit_out %in% valid_units) {
    rlang::abort("Invalid 'unit_out' argument.")
  }

  # Check input parameter 'source'
  if (!source %in% c("imf_weo", "wb_wdi") & !exists(source, mode = "list")) {
    rlang::abort("Invalid 'source' argument. Has to be either 'imf_weo', 'wb_wdi' or valid custom source.")
  }

  if (!is.logical(verbose)) {
    rlang::abort("Invalid 'verbose' argument. Has to be either TRUE or FALSE.")
  }
return(gdp)
}
