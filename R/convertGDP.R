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
#'   package to be installed. The data-frame needs to have at least 2 columns, in some cases 3:
#'   \itemize{
#'     \item a character column with iso3c
#'     ([wikipedia](https://en.wikipedia.org/wiki/ISO_3166-1_alpha-3)) country codes,
#'     \item a numeric column with years (only required when converting from or to current currencies),
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
#'     \item "constant YYYY €"
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
#'     \item "constant YYYY €"
#'   }
#'   where YYYY should be replaced with a year e.g. "2010" or "2017".
#' @param source A string referring to a package internal data frame containing the conversion factors, or
#'   a data-frame that exists in the calling environment.
#'   Use [print_source_info()](https://pik-piam.github.io/GDPuc/reference/print_source_info.html)
#'   to learn about the available sources.
#' @param use_USA_deflator_for_all TRUE or FALSE (default). If TRUE, then only the USA deflator is used to adjust for
#'   inflation, regardless of the country codes provided. This is a very specific deviation from the correct conversion
#'   process, which nevertheless is often used in the integrated assessment community. Use carefully!
#' @param with_regions NULL or a data-frame. The data-frame should be "country to region
#'   mapping": one column named "iso3c" with iso3c country codes, and one column named
#'   "region" with region codes to which the countries belong. Any regions in the gdp
#'   object will then be disaggregated according to the region mapping and weighed by the
#'   GDP share of countries in that region in the year of the unit, converted on a country
#'   level, and re-aggregated before being returned.
#' @param replace_NAs NULL by default, meaning no NA replacement. Can be set to one of the following:
#'   \itemize{
#'     \item 0: resulting NAs are simply replaced with 0.
#'     \item NA: resulting NAs are explicitly kept as NA.
#'     \item "no_conversion": resulting NAs are simply replaced with the values from the gdp argument.
#'     \item "linear": missing conversion factors in the source object are inter- and extrapolated linearly.
#'     For the extrapolation, the closest 5 data points are used.
#'     \item "regional_average": missing conversion factors in the source object are replaced with
#'     the regional average of the region to which the country belongs. This requires a region-mapping to
#'     be passed to the function, see the with_regions argument.
#'     \item "with USA": missing conversion factors in the source object are replaced with
#'     the conversion factors of the USA.
#'   }
#'   Can also be a vector with "linear" as first element, e.g. c("linear", 0) or c("linear", "no_conversion"),
#'   in which case, the operations are done in sequence.
#' @param verbose TRUE or FALSE. A flag to turn verbosity on or off. Be default it is equal to the
#'   GDPuc.verbose option, which is FALSE if not set to TRUE by the user.
#' @param return_cfs TRUE or FALSE. Set to TRUE to additionally return a tibble with the conversion factors
#'   used. In that case a list is returned with the converted GDP under "result", and the conversion factors
#'   used under "cfs".
#' @return The gdp argument, with the values in the "value" column, converted to unit_out. If the argument
#'   return_cfs is TRUE, then a list is returned with the converted GDP under "result", and the conversion
#'   factors used under "cfs".
#' @seealso The [countrycode](https://github.com/vincentarelbundock/countrycode) package to convert country codes.
#' @examples
#'   my_tbble <- tibble::tibble(iso3c = "FRA",
#'                              year = 2013,
#'                              value = 100)
#'
#'   convertGDP(gdp = my_tbble,
#'              unit_in = "current LCU",
#'              unit_out = "constant 2015 Int$PPP")
#'
#' @export
convertGDP <- function(gdp,
                       unit_in,
                       unit_out,
                       source = "wb_wdi",
                       use_USA_deflator_for_all = FALSE,
                       with_regions = NULL,
                       replace_NAs = NULL,
                       verbose = getOption("GDPuc.verbose", default = FALSE),
                       return_cfs = FALSE) {
  # Save all function arguments as list
  arg <- as.list(environment())

  # Check function arguments
  do.call(check_user_input, arg)

  # Set GDPuc.verbose option to the verbose argument, while in this function
  withr::local_options(list("GDPuc.verbose" = verbose))

  # Return straight away if no conversion is needed
  if (identical(unit_in, unit_out)) {
    cli_inform(function() cli::cli_alert_info("No conversion: unit_in = unit_out."))
    return(gdp)
  }

  # Transform user input for internal use, while performing some last consistency checks
  internal <- transform_user_input(gdp, unit_in, unit_out, source, use_USA_deflator_for_all, with_regions, replace_NAs)

  # Avoid NOTE in package check for CRAN
  . <- NULL
  # Get appropriate function
  f <- paste0(internal$unit_in, "_2_", internal$unit_out) %>%
    gsub(" ", "_", .) %>%
    gsub("_YYYY", "", .) %>%
    gsub("\\$", "", .) %>%
    # \u20ac is the ascii code for the € sign
    gsub("\u20ac", "EURO", .)

  # Get list of function arguments
  a <- list("gdp" = internal$gdp, "source" = internal$source) %>%
    {if ("base_x" %in% names(internal)) c(., "base_x" = internal$base_x) else .} %>%
    {if ("base_y" %in% names(internal)) c(., "base_y" = internal$base_y) else .}


  cli_inform(function() cli::cli_alert_info(
    "Converting GDP with conversion factors from {crayon::blue(internal$source_name)}:")
  )

  # Call function
  x <- do.call(f, a)

  # Handle NAs
  if (!is.null(replace_NAs) && 0 %in% replace_NAs) x[is.na(x)] <- 0
  if (any(is.na(x$value) & !is.na(internal$gdp$value))) {
    if (!is.null(replace_NAs)) {
      if ("no_conversion" %in% replace_NAs) {
        x$value[is.na(x$value)] <- internal$gdp$value[is.na(x$value)]
      }
    } else {
      warn("NAs have been generated for countries lacking conversion factors!")
    }
  }

  # Return with original type and names
  x <- transform_internal(x, gdp, with_regions, internal$require_year_column)

  if (return_cfs) {
    return(list("result" = x, "cfs" = do.call(get_conversion_factors, arg[1:7])))
  } else {
    return(x)
  }
}

#' @describeIn convertGDP Short cut for `convertGDP(..., source = "wb_wdi_cpi")`
#' @param ... Arguments passed on to `convertGDP()`
#' @examples
#'   # Convert using the CPI as deflator.
#'   convertGDP(gdp = my_tbble,
#'              unit_in = "current LCU",
#'              unit_out = "constant 2015 Int$PPP",
#'              source = "wb_wdi_cpi")
#'   # Or using the shortcut `convertCPI()`
#'   convertCPI(gdp = my_tbble,
#'              unit_in = "current LCU",
#'              unit_out = "constant 2015 Int$PPP")
#'
#' @export
convertCPI <- function(...) convertGDP(..., source = "wb_wdi_cpi")

#' @describeIn convertGDP Convert a single value, while specifying iso3c code and year. Simpler than creating a
#'   single row tibble.
#' @param x Number to convert
#' @param iso3c Country code
#' @param year NULL, or year of value. Only plays a role when converting from or to current currencies.
#' @examples
#'   # Convert a single value quickly
#'   convertSingle(x = 100,
#'                 iso3c = "FRA",
#'                 year = 2013,
#'                 unit_in = "current LCU",
#'                 unit_out = "constant 2015 Int$PPP")
#' @export
convertSingle <- function(x, iso3c, year = NULL, ...) {
  tib <- tibble::tibble("iso3c" = iso3c, "value" = x)

  if (!is.null(year)) {
    tib <- tibble::add_column(tib, "year" = year, .before = "value")
  }

  tib_c <- convertGDP(gdp = tib, ...)

  if (tibble::is_tibble(tib_c)) {
    return(tib_c$value)
  } else {
    return(tib_c)
  }
}
