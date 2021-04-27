#' Make sure that user input is valid
#'
#' check_user_input performs some checks on the function arguments.
#'
#' @inheritParams convertGDP
#'
#' @return TRUE or an Error
check_user_input <- function(gdp, unit_in, unit_out, source, with_regions, verbose) {

  # Check if magclass package is required
  if (class(gdp)[1] == "magpie" && !requireNamespace("magclass", quietly = TRUE)) {
    rlang::abort("Missing package. Please install the 'magclass' package to convert magpie objects.")
  }

  # Check 'gdp' input parameter
  if (is.data.frame(gdp)) {
    # Convert factor columns to character columns
    gdp <- gdp %>%
      dplyr::mutate(dplyr::across(tidyselect::vars_select_helpers$where(is.factor), as.character)) %>%
      tibble::as_tibble()

    if (! "value" %in% colnames(gdp)) {
      rlang::abort("Invalid 'gdp' argument. `gdp` does not have the required 'value' column.")
    }
    if (! "iso3c" %in% colnames(gdp)) {
      i_iso3c <- gdp %>%
        dplyr::select(tidyselect::vars_select_helpers$where(
          ~ is.character(.x) & nchar(.x[[1]]) == 3
        )) %>%
        colnames()
      if (identical(i_iso3c, character(0))) {
        rlang::abort("Invalid 'gdp' argument. `gdp` has no 'iso3c' column, and no other \\
                     column could be identified in its stead.")
      }
      rlang::warn(glue::glue("No 'iso3c' column in 'gdp' argument. Using '{i_iso3c}' column instead."))
    }
    if (! "year" %in% colnames(gdp)) {
      i_year <- gdp %>%
        dplyr::select(tidyselect::vars_select_helpers$where(
          ~ is.numeric(.x) & !is.na(.x[[1]]) & nchar(as.character(.x[[1]])) == 4
        )) %>%
        colnames()
      if (identical(i_year, character(0))) {
        rlang::abort(glue::glue("Invalid 'gdp' argument. 'gdp' does not have the required \\
                                'year' column, and no other column could be identified in its stead."))
      }
      rlang::warn(glue::glue("No 'year' column in 'gdp' argument. Using '{i_year}' column instead."))
    }

  } else if (class(gdp) == "magpie"){
    # Check if there is years info
    if(is.null(magclass::getYears(gdp))){
      rlang::abort("No year information in mag object!")
    }
  } else {
    rlang::abort("Invalid 'gdp' argument. `gdp` is neither a data-frame, a tibble nor a 'magpie' object.")
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
  if (!source %in% c("imf_weo", "wb_wdi") & !exists(source, mode = "list")) {
    rlang::abort("Invalid 'source' argument. Has to be either 'imf_weo', 'wb_wdi' or valid custom source.")
  }

  # Check input parameter 'with_regions'
  if (!is.null(with_regions)) {
    if (!length(with_regions) != 2) {
      rlang::abort("Invalid 'with_regions' argument. Has to be either 'NULL', or a dataframe of length 2.")
    }
    if (!all(c("iso3c", "region") %in% colnames(with_regions))) {
      rlang::abort("Invalid 'with_regions' argument. Needs to have columns 'iso3c' and 'region'.")
    }
    if (grepl("current", unit_in) || grepl("current", unit_out)) {
      rlang::abort("'Current' GDP units are not compatible with regional aggregation.")
    }
  }

  # Check input parameter 'verbose'
  if (!is.logical(verbose)) {
    rlang::abort("Invalid 'verbose' argument. Has to be either TRUE or FALSE.")
  }

  TRUE
}
