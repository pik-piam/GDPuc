#' Convert GDP time series data from one unit to another
#'
#'   # All on country level.
#' 0# Converting current LCU (nominal) to constant LCU with base year x (real)
#' Need GDP deflator with base year x, unit = [.LCU/xLCU], often given in %
#' Divide nominal GDP by deflator in every year

#' 1# Changing base year of constant LCU from x to y
#' Need either deflator with base year x, or current LCU series
#' With deflator: Multiply constant LCU with base year x [xLCU] with value in y
#'                of deflator with base year x [yLCU/xLCU]
#' With current series: Compute deflator (nominal divided by real), then follow above

#' 2# Converting current LCU to current Int$MER or current Int$PPP
#' Need yearly Market exchange rates, or yearly PPPs
#' Multiply nominal GDP by chosen conversion factor

#' 3# Converting constant LCU to constant Int$MER or constant Int$PPP, all same base year
#' Need base year Market exchange rates, or PPPs
#' Multiply real GDP by chosen conversion factor

#' 3# Converting current LCU to constant Int$MER or constant Int$PPP, all same base year
#' Need base year Market exchange rates, or PPPs
#' Multiply real GDP by chosen conversion factor

#' 4# Converting constant Int$MER to constant Int$PPP with base year x
#' Need MER and PPP of base year

#' 5# Converting current Int$PPP to constant Int$PPP with base year x
#' Need yearly PPP conversion rate, GDP deflator with base year x
#' Divide current Int$PPP by PPP conversion rate, then follow 0, and 3

#' ## Converting current LCU to Int$PPP Need yearly PPPs,
#'
#' @param gdp A tibble with 3 columns "iso3c", "year" and "value".
#' @param unit_in A string with the incoming GDP unit.
#' @param unit_out A string with the desired output GDP unit.
#' @param source A string indicating the source database to use for conversion
#'   factors.
#' @param verbose TRUE or FALSE
#'
#' @importFrom magrittr %>%
#'
#' @return A tibble with 3 columns
#' @export
convertGDP <- function(gdp, unit_in, unit_out, source = "wb_wdi", verbose = FALSE) {

  # Check function arguments
  check_user_input(gdp, unit_in, unit_out, source, verbose)

  # Extract base years if they exist, and adjust string
  if (grepl("constant", unit_in)) {
    base_x <- stringr::str_match(unit_in, "constant (....)")[,2] %>% as.double()
    unit_in <- sub(base_x, "YYYY", unit_in) %>%
      paste0(" base x")
  }
  if (grepl("constant", unit_out)) {
    base_y <- stringr::str_match(unit_out, "constant (....)")[,2] %>% as.double()
    unit_out <- sub(base_y, "YYYY", unit_out) %>%
      paste0(" base y")
  }

  # Get appropriate function
  f <- paste0(unit_in, "_2_", unit_out) %>%
    stringr::str_replace_all(c(
      " " = "_",
      "_YYYY"="",
      "\\$"=""
    ))

  # Get list of function arguments
  this_e <- environment()
  a <- list("gdp" = gdp, "source" = source) %>%
    {if (exists("base_x", envir = this_e, inherits = FALSE)) c(., "base_x" = base_x) else .} %>%
    {if (exists("base_y", envir = this_e, inherits = FALSE)) c(., "base_y" = base_y) else .}

  # Call function
  x <- do.call(f, a)

  return(x)
}
