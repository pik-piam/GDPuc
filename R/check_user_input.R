#' Make sure that user input is valid
#'
#' check_user_input performs some checks on the function arguments.
#'
#' @inheritParams convertGDP
#'
#' @return TRUE or an Error
check_user_input <- function(gdp, unit_in, unit_out, source, verbose) {

  # Check 'gdp' input parameter
  if (!tibble::is_tibble(gdp)) {
    rlang::abort("Invalid 'gdp' argument. `gdp` is not a tibble.")
  }
  if (!all(colnames(gdp) %in% c("iso3c", "year", "value"))) {
    rlang::abort("Invalid 'gdp' argument. `gdp` does not have the correct columns.")
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
  if (!source %in% c("imf_weo", "wb_wdi")) {
    rlang::abort("Invalid 'source' argument. Has to be either 'imf_weo' or 'wb_wdi.")
  }

  if (!is.logical(verbose)) {
    rlang::abort("Invalid 'verbose' argument. Has to be either TRUE or FALSE.")
  }

  TRUE
}
