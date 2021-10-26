#' Convert GDP data
#'
#' @description
#' `r lifecycle::badge("stable")`
#'
#' convertGDP() converts GDP time series data from one unit to another, using GDP
#' deflators, market exchange rates (MERs) and purchasing power parity
#' conversion factors (PPPs).
#'
#' @details
#' When providing a custom source to the function, a certain format is required.
#' The source object must be a data frame or tibble with at least the following columns:
#' \itemize{
#'    \item a character column named "iso3c" with iso3c
#'     ([wikipedia](https://en.wikipedia.org/wiki/ISO_3166-1_alpha-3)) country codes,
#'    \item a numeric column named "year" with years,
#'    \item a numeric column named "GDP deflator" with values of the GDP deflator divided
#'    by 100 (so that in the base year the GDP deflator is equal to 1, not 100).
#'    The base year of the deflator can be any year, and can be country-specific.
#'    \item a numeric column named "MER (LCU per US$)" with MER values,
#'    \item a numeric column named "PPP conversion factor, GDP (LCU per international $)"
#'    wit PPP exchange rate values.
#'  }
#'
#' @param gdp A tibble, data frame or magpie object, the latter of which
#'   requires the [magclass](https://github.com/pik-piam/magclass)
#'   package to be installed. The data-frame needs to have at least 3 columns:
#'   \itemize{
#'     \item a character column with iso3c
#'     ([wikipedia](https://en.wikipedia.org/wiki/ISO_3166-1_alpha-3)) country codes,
#'     \item a numeric column with years,
#'     \item a numeric column named "value" with GDP values.
#'  }
#' @param unit_in A string with the incoming GDP unit, one of:
#'   \itemize{
#'     \item "current LCU"
#'     \item "current Int$PPP"
#'     \item "current US$MER"
#'     \item "constant YYYY LCU"
#'     \item "constant YYYY Int$PPP"
#'     \item "constant YYYY US$MER"
#'   }
#'   where YYYY should be replaced with a year e.g. "2010" or "2017".
#' @param unit_out A string with the outgoing GDP unit, one of:
#'   \itemize{
#'     \item "current LCU"
#'     \item "current Int$PPP"
#'     \item "current US$MER"
#'     \item "constant YYYY LCU"
#'     \item "constant YYYY Int$PPP"
#'     \item "constant YYYY US$MER"
#'   }
#'   where YYYY should be replaced with a year e.g. "2010" or "2017".
#' @param source A data frame to use for conversion factors. Can be a custom data-frame
#'   that exists in the calling environment, or one of the package internal ones.
#'   Use [print_source_info()](https://johanneskoch94.github.io/GDPuc/reference/print_source_info.html)
#'   to learn about the available sources.
#' @param with_regions NULL or a data-frame. The data-frame should be "country to region
#'   mapping": one column named "iso3c" with iso3c country codes, and one column named
#'   "region" with region codes to which the countries belong. Any regions in the gdp
#'   object will then be disaggregated according to the region mapping and weighed by the
#'   GDP share of countries in that region in the year of the unit, converted on a country
#'   level, and re-aggregated before being returned.
#' @param replace_NAs `r lifecycle::badge("maturing")`
#'   NULL, 0, 1 or "regional_average". Should countries for which conversion factors are
#'   missing, have their NA-conversion factors replaced with 1 or with a regional average?
#'   The default is no. If 0, then resulting NAs are simply replaced with 0. If 1, then the
#'   missing conversion factors will be set to 1, and essentially, no conversion will take
#'   place. If "regional_average" then, the regional average of the region to which the country
#'   belongs to will be used. This requires a region-mapping to be passed to the function,
#'   see the with_regions argument.
#' @param verbose TRUE or FALSE. A flag to turn verbosity on or off. Overrules
#'   the GDPuc.verbose option, if it is set.
#' @return The gdp argument, with the values in the "value" column, converted to unit_out.
#' @seealso The [countrycode](https://github.com/vincentarelbundock/countrycode)
#'   package to convert country codes.
#' @importFrom magrittr %>%
#' @export
convertGDP <- function(gdp,
                       unit_in,
                       unit_out,
                       source = "wb_wdi",
                       with_regions = NULL,
                       replace_NAs = NULL,
                       verbose = FALSE) {
  # Avoid NOTE in check
  . <- NULL
  # Capture the source argument as quosure
  source <- rlang::enquo(source)

  # Check function arguments
  check_user_input(gdp, unit_in, unit_out, source, with_regions, replace_NAs, verbose)

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
  internal <- transform_user_input(gdp, unit_in, unit_out, source, with_regions, replace_NAs)

  # Get appropriate function
  f <- paste0(internal$unit_in, "_2_", internal$unit_out) %>%
    stringr::str_replace_all(c(
      " " = "_",
      "_YYYY" = "",
      "\\$" = ""
    ))

  # Get list of function arguments
  a <- list("gdp" = internal$gdp, "source" = internal$source) %>%
    {if ("base_x" %in% names(internal)) c(., "base_x" = internal$base_x) else .} %>%
    {if ("base_y" %in% names(internal)) c(., "base_y" = internal$base_y) else .}


  cli_inform(function() cli::cli_alert_info(
    "Converting GDP with conversion factors from {crayon::blue(internal$source_name)}:")
  )

  # Call function
  x <- do.call(f, a)

  if (!is.null(replace_NAs) && replace_NAs == 0) x[is.na(x)] <- 0
  if (any(is.na(x$value) & !is.na(internal$gdp$value))) {
    warn("NAs have been generated for countries lacking conversion factors!")
  }

  # Return with original type and names
  x <- transform_internal(x, gdp, with_regions)

  return(x)
}
