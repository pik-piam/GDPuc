#' Make sure that user input is valid
#'
#' check_user_input performs some checks on the function arguments.
#'
#' @inheritParams convertGDP
#'
#' @return TRUE or an Error
check_user_input <- function(gdp, unit_in, unit_out, source, with_regions, verbose) {

  # Check 'gdp' input parameter
  if (is.data.frame(gdp)) {
    if (! "value" %in% colnames(gdp)) {
      abort("Invalid 'gdp' argument. `gdp` does not have the required 'value' column.")
    }
    if (length(gdp) < 3) {
      abort("Invalid 'gdp' argument. `gdp` must have at least 3 columns.")
    }
  } else if (class(gdp) == "magpie"){
    # Check for magclass package
    if (!requireNamespace("magclass", quietly = TRUE)) {
      abort("Missing 'magclass' package. Please install 'magclass' to convert magpie objects.")
    }
    # Check if there is years info
    if(is.null(magclass::getYears(gdp))){
      abort("No year information in mag object!")
    }
  } else {
    abort("Invalid 'gdp' argument. `gdp` is neither a data-frame nor a 'magpie' object.")
  }

  # Check input parameters 'unit_in' and 'unit_out'
  valid_units <- c(
    "current LCU",
    "current US\\$MER",
    "current Int\\$PPP",
    "constant .... LCU",
    "constant .... US\\$MER",
    "constant .... Int\\$PPP"
  )
  if (!is.character(unit_in) || !any(sapply(valid_units, grepl, unit_in))) {
    abort("Invalid 'unit_in' argument.")
  }
  if (!is.character(unit_out) || !any(sapply(valid_units, grepl, unit_out))) {
    abort("Invalid 'unit_out' argument.")
  }

  # Check input parameter 'source'
  if (!source %in% c("imf_weo", "wb_wdi") && !exists(source, mode = "list")) {
    abort("Invalid 'source' argument. Has to be either one of the internal sources, \\
          or valid custom source.")
  }
  required_cols_in_source <- c(
    "iso3c",
    "year",
    "GDP deflator",
    "MER (LCU per US$)",
    "PPP conversion factor, GDP (LCU per international $)"
  )
  if (!all(required_cols_in_source %in% colnames(eval(rlang::sym(source))))) {
    abort("Invalid 'source' argument. Has to contain at least following columns: \\
          {paste(required_cols_in_source, collapse = '; ')}")
  }

  # Check input parameter 'with_regions'
  if (!is.null(with_regions)) {
    if (!is.data.frame(with_regions) || length(with_regions) != 2) {
      abort("Invalid 'with_regions' argument. Has to be either 'NULL', \\
            or a data.frame of length 2.")
    }
    if (!all(c("iso3c", "region") %in% colnames(with_regions))) {
      abort("Invalid 'with_regions' argument. Needs to have columns 'iso3c' and 'region'.")
    }
    if (grepl("LCU", unit_in) || grepl("LCU", unit_out)) {
      abort("'LCU' GDP units are not compatible with regional aggregation.")
    }
    if (!any(grepl("GDP, PPP \\(constant .... international \\$\\)",
               colnames(eval(rlang::sym(source)))))) {
      abort("Incompatible source. Source requires a column of type \\
            'GDP, PPP (constant YYYY international $)'")
    }
  }

  # Check input parameter 'verbose'
  if (!is.logical(verbose)) {
    abort("Invalid 'verbose' argument. Has to be either TRUE or FALSE.")
  }

  TRUE
}
