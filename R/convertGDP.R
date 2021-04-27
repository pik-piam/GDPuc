#' Convert GDP time series data from one unit to another
#'
#' convertGDP converts GDP time series data from one unit to another, using GDP
#' deflators, market exchange rates (MERs) and purchasing power parity
#' conversion factors (PPPs) from `source`.
#'
#' @param gdp A tibble or data-frame with at least 3 columns containing iso3c
#'   country codes, years, and GDP values.
#' @param unit_in A string with the incoming GDP unit.
#' @param unit_out A string with the desired output GDP unit.
#' @param source A string indicating the source database to use for conversion
#'   factors.
#' @param with_regions Data-frame or tibble with region mapping
#' @param verbose TRUE or FALSE
#'
#' @importFrom magrittr %>%
#' @return A tibble with 3 columns
#' @export
convertGDP <- function(gdp,
                       unit_in,
                       unit_out,
                       source = "wb_wdi",
                       with_regions = NULL,
                       verbose = FALSE) {

  # Check function arguments
  check_user_input(gdp, unit_in, unit_out, source, with_regions, verbose)

  # Set verbose option, if necessary
  if (verbose != getOption("GDPuc.verbose", default = FALSE)) {
    old_value <- getOption("GDPuc.verbose", default = FALSE)
    options("GDPuc.verbose" = verbose)
    on.exit(options("GDPuc.verbose" = old_value))
  }

  # Return straight away if no conversion is needed
  if (identical(unit_in, unit_out)) {
    cli_inform(function() cli::cli_alert_info("No conversion: unit_in = unit_out."))
    return(gdp)
  }

  # Convert to tibble, if necessary
  if (class(gdp)[1] == "magpie"){
    return_mag <- TRUE
    gdp <- mag2tibb(gdp)
  } else return_mag <- FALSE
  if (class(gdp)[1] == "data.frame") {
    return_df <- TRUE
    i_factors <- gdp %>%
      dplyr::select(tidyselect::vars_select_helpers$where(is.factor)) %>%
      colnames()
    gdp <- gdp %>%
      dplyr::mutate(dplyr::across(tidyselect::all_of(i_factors), as.character)) %>%
      tibble::as_tibble()
  } else return_df <- FALSE

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
      dplyr::select(tidyselect::vars_select_helpers$where(
        ~ is.character(.x) & nchar(.x[[1]]) == 3
      )) %>%
      colnames()
    gdp <- gdp %>%
      dplyr::rename("iso3c" = !!rlang::sym(i_iso3c)) %>%
      dplyr::arrange("iso3c", 1)
  }
  if (! "year" %in% colnames(gdp)) {
    i_year <- gdp %>%
      dplyr::select(tidyselect::vars_select_helpers$where(
        ~ is.numeric(.x) & !is.na(.x[[1]]) & nchar(as.character(.x[[1]])) == 4
      )) %>%
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
  cli_inform(function() cli::cli_alert_info(
    "Converting GDP with conversion factors from {crayon::blue(source)}:")
  )

  x <- do.call(f, a)

  # Return with original names, if changed
  if (exists("i_iso3c", envir = this_e)) {
    x <- x %>% dplyr::rename(!!rlang::sym(i_iso3c) := "iso3c")
  }
  if (exists("i_year", envir = this_e)) {
    x <- x %>% dplyr::rename(!!rlang::sym(i_year) := "year")
  }

  # Return as original object if necessary
  if (return_mag){
    x <- magclass::as.magpie(x[,-1], spatial="iso3c", temporal="year")
    if(any(is.na(x))) {
      rlang::warn("NAs may have been generated for countries lacking conversion factors!")}
  }
  if (return_df){
    x <- x %>%
      dplyr::mutate(dplyr::across(tidyselect::all_of(i_factors), as.factor)) %>%
      as.data.frame()
  }


  return(x)
}
