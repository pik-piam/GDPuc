# Make sure that user input is valid
#
# check_user_input performs some checks on the function arguments.
# @return TRUE or an Error
check_user_input <- function(gdp, unit_in, unit_out, source, with_regions, replace_NAs, verbose, return_cfs) {

  # Check the gdp argument
  check_gdp(gdp)
  # Check the unit_in and unit_out arguments
  check_unit_in_out(unit_in, unit_out)
  # Check the source argument
  source <- check_source(source)
  # Check the with_regions argument. Depends on unit_in, unit_out, and the source tibble
  check_with_regions(unit_in, unit_out, source, with_regions)
  # Check the replaceNAs argument. Depends on the with_regions argument.
  check_replace_NAs(with_regions, replace_NAs)
  # Check the verbose argument
  check_verbose(verbose)
  # Check the return_cfs argument
  check_return_cfs(return_cfs)

  TRUE
}



# Check 'gdp' input parameter
check_gdp <- function(gdp) {
  if (is.data.frame(gdp)) {
    if (! "value" %in% colnames(gdp)) {
      abort("Invalid 'gdp' argument. 'gdp' does not have the required 'value' column.")
    }
    if (!is.numeric(gdp$value)) {
      abort("Invalid 'gdp' argument. The 'value' column is not numeric.")
    }
    if (length(gdp) < 3) {
      abort("Invalid 'gdp' argument. 'gdp' must have at least 3 columns.")
    }
  } else if (inherits(gdp, "magpie")) {
    # Check for magclass package
    rlang::check_installed("magclass", reason = "in order for magpie objects to be recognized.")
    # Check if there is years info
    if (is.null(magclass::getYears(gdp))) {
      # I need at least one explizit call to a crayon:: function... otherwise I get a CRAN note
      h <- crayon::bold("magpie")
      abort("Invalid 'gdp' argument. No year information in {h} object.")
    }
  } else {
    abort("Invalid 'gdp' argument. 'gdp' is neither a data-frame nor a 'magpie' object.")
  }
}


# Check input parameters 'unit_in' and 'unit_out'
check_unit_in_out <- function(unit_in, unit_out) {
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
}

# Check input parameter 'source'
check_source <- function(source) {
  if (!is.data.frame(source)) {
    if (is.character(source)) {
      if (exists(source, environment())) {
        source <- rlang::eval_tidy(rlang::sym(source))
      } else {
        abort("Invalid 'source' argument. If 'source' is a string, it must be one of the internal sources. \\
              Use print_source_info() for information on available sources. \\
              If you are trying to pass a custom source, pass the data frame directly, not its name.")
      }
    } else {
      abort("Invalid 'source' argument. 'source' is neither a data frame nor a string.")
    }
  }

  required_cols_in_source <- c(
    "iso3c",
    "year",
    "GDP deflator",
    "MER (LCU per US$)",
    "PPP conversion factor, GDP (LCU per international $)"
  )
  if (!all(required_cols_in_source %in% colnames(source))) {
    abort("Invalid 'source' argument. Required columns are: {paste(required_cols_in_source, collapse = '; ')}")
  }
  source
}

# Check input parameter 'with_regions'
check_with_regions <- function(unit_in, unit_out, source, with_regions) {
  if (!is.null(with_regions)) {
    if (!is.data.frame(with_regions) || length(with_regions) != 2) {
      abort("Invalid 'with_regions' argument. Has to be either 'NULL', or a data.frame of length 2.")
    }
    if (!all(c("iso3c", "region") %in% colnames(with_regions))) {
      abort("Invalid 'with_regions' argument. Needs to have columns 'iso3c' and 'region'.")
    }
    if (grepl("LCU", unit_in) || grepl("LCU", unit_out)) {
      abort("'LCU' GDP units are not compatible with regional aggregation.")
    }
    if (!any(grepl("GDP, PPP \\(constant .... international \\$\\)", colnames(source)))) {
      abort("Incompatible source. Source requires a column of type 'GDP, PPP (constant YYYY international $)'")
    }
  }
}


# Check input parameter 'replace_NAs'
check_replace_NAs <- function(with_regions, replace_NAs) {
  if (!is.null(replace_NAs)) {
    if (setequal(replace_NAs, 1)) {
      lifecycle::deprecate_warn("0.7.0", "convertGDP(replace_NAs = 'should not be 1')")
    }
    if ("linear_regional_average" %in% replace_NAs) {
      lifecycle::deprecate_stop(
        "0.8.0",
        "convertGDP(replace_NAs = '\"linear_regional_average\" has been replaced by c(\"linear\", \"regional_average\")')"
      )
    }
    if (!all(replace_NAs %in% c(NA, 0, 1, "no_conversion", "linear", "regional_average"))) {
      abort("Invalid 'replace_NAs' argument. Has to be either NULL, NA, 0, 1, no_conversion, linear, regional_average or \\
            a combination of the above.")
    }
    if (length(replace_NAs) > 1 && replace_NAs[1] != "linear") {
      abort("Invalid 'replace_NAs' argument. The only accepted combinations of arguments start with 'linear', e.g. \\
            c('linear', 'no_conversion').")
    }
    if ("regional_average" %in% replace_NAs && is.null(with_regions)) {
      abort("Using 'regional_average' requires a region mapping. The 'with_regions' argument can't be NULL.")
    }
  }
}

# Check input parameter 'verbose'
check_verbose <- function(verbose) {
  if (!is.logical(verbose)) {
    abort("Invalid 'verbose' argument. Has to be either TRUE or FALSE.")
  }
}

# Check input parameter 'return_cfs'
check_return_cfs <- function(return_cfs) {
  if (!is.logical(return_cfs)) {
    abort("Invalid 'return_cfs' argument. Has to be either TRUE or FALSE.")
  }
}
