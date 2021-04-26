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
#' @return A tibble with 3 columns
#' @export
convertGDP <- function(gdp,
                       unit_in,
                       unit_out,
                       source = "wb_wdi",
                       verbose = FALSE) {
  return_mag <- FALSE
  if(is.magpie(gdp)){ return_mag <- TRUE}
  # Check function arguments
  gdp <- check_user_input(gdp, unit_in, unit_out, source, verbose)

  # Return straight away if no conversion is needed
  if (identical(unit_in, unit_out)) {
    return(gdp)
  }

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

  # Rename columns if necessary
  if (! "iso3c" %in% colnames(gdp)) {
    i_iso3c <- gdp %>%
      dplyr::select(tidyselect:::where(~is.character(.x) & nchar(.x[[1]]) == 3)) %>%
      colnames()
    gdp <- gdp %>%
      dplyr::rename("iso3c" = !!rlang::sym(i_iso3c)) %>%
      dplyr::arrange("iso3c", 1)
  }
  if (! "year" %in% colnames(gdp)) {
    i_year <- gdp %>%
      dplyr::select(tidyselect:::where(~is.numeric(.x) &
                                         !is.na(.x[[1]]) &
                                         nchar(as.character(.x[[1]])) == 4)) %>%
      colnames()
    gdp <- gdp %>%
      dplyr::rename("year" = !!rlang::sym(i_year)) %>%
      dplyr::arrange("year", 2)
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

  # Return with original names, if changed
  if (exists("i_iso3c", envir = this_e)) {
    x <- x %>% dplyr::rename(!!rlang::sym(i_iso3c) := "iso3c")
  }
  if (exists("i_year", envir = this_e)) {
    x <- x %>% dplyr::rename(!!rlang::sym(i_year) := "year")
  }

  if (return_mag==TRUE){
    x <- as.magpie(x[,-1], spatial="iso3c", temporal="year")
    if(any(is.na(x))) {
      warning("NAs may have been generated for countries lacking conversion factors!")}
  }


  return(x)
}
