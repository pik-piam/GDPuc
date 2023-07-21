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
#' @param source A string referring to a package internal data frame containing the conversion factors, or
#'   a data-frame that exists in the calling environment.
#'   Use [print_source_info()](https://pik-piam.github.io/GDPuc/reference/print_source_info.html)
#'   to learn about the available sources.
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
  internal <- transform_user_input(gdp, unit_in, unit_out, source, with_regions, replace_NAs)

  # Avoid NOTE in package check for CRAN
  . <- NULL
  # Get appropriate function
  f <- paste0(internal$unit_in, "_2_", internal$unit_out) %>%
    gsub(" ", "_", .) %>%
    gsub("_YYYY", "", .) %>%
    gsub("\\$", "", .)

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
  x <- transform_internal(x, gdp, with_regions)

  if (return_cfs) {
    return(list("result" = x, "cfs" = do.call(get_conversion_factors, arg[1:6])))
  } else {
    return(x)
  }
}
