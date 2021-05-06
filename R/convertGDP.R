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
#' @param with_regions NULL or a data-frame or tibble with region mapping
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
  . = NULL

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

  # Transform user input for internal use, while performing some last consistency checks
  internal <- transform_user_input(gdp, unit_in, unit_out, source, with_regions)

  # Get appropriate function
  f <- paste0(internal$unit_in, "_2_", internal$unit_out) %>%
    stringr::str_replace_all(c(
      " " = "_",
      "_YYYY"="",
      "\\$"=""
    ))

  # Get list of function arguments
  a <- list("gdp" = internal$gdp, "source" = source) %>%
    {if ("base_x" %in% names(internal)) c(., "base_x" = internal$base_x) else .} %>%
    {if ("base_y" %in% names(internal)) c(., "base_y" = internal$base_y) else .}


  cli_inform(function() cli::cli_alert_info(
    "Converting GDP with conversion factors from {crayon::blue(source)}:")
  )

  # Call function
  x <- do.call(f, a)

  if(any(is.na(x$value) & !is.na(internal$gdp$value))) {
    warn("NAs have been generated for countries lacking conversion factors!")
  }

  # Return with original type and names
  x <- transform_internal(x, gdp, with_regions)

  return(x)
}
