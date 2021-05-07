#' Transform user input for package internal use
#'
#' @inheritParams convertGDP
#'
#' @return List
transform_user_input <- function(gdp, unit_in, unit_out, source, with_regions) {

  . = NULL

  # Convert to tibble, if necessary
  if (class(gdp)[1] == "magpie"){
    gdp <- mag2tibb(gdp)
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
      dplyr::select(tidyselect::vars_select_helpers$where(
        ~ (is.character(.x) || is.factor(.x)) && nchar(as.character(.x[[1]])) == 3
      )) %>%
      colnames()

    if (identical(i_iso3c, character(0))) {
      abort("Invalid 'gdp' argument. `gdp` has no 'iso3c' column, and no other \\
               column could be identified in its stead.")
    }

    warn("No 'iso3c' column in 'gdp' argument. Using '{i_iso3c}' column instead.")

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

    if (identical(i_year, character(0))) {
      abort("Invalid 'gdp' argument. 'gdp' does not have the required \\
               'year' column, and no other column could be identified in its stead.")
    }

    warn("No 'year' column in 'gdp' argument. Using '{i_year}' column instead.")

    gdp <- gdp %>%
      dplyr::rename("year" = !!rlang::sym(i_year)) %>%
      dplyr::arrange("year", 2)
  }


  # Dissagregate to country level if with_regions is activated
  if (!is.null(with_regions)) {
    weight_unit <- stringr::str_match(unit_in, "\\$(...)")[2]
    weight_year <- base_x
    with_regions <- dplyr::rename(with_regions, "gdpuc_region" = .data$region)
    gdp <- disaggregate_regions(gdp, with_regions, weight_unit, weight_year, source)
  }


  # Check availability of required conversion factors in source
  if (length(intersect(unique(gdp$year), unique(eval(rlang::sym(source))$year))) == 0) {
    abort("No information in source {crayon::bold(source)} for years in 'gdp'.")
  }
  if (length(intersect(unique(gdp$iso3c), unique(eval(rlang::sym(source))$iso3c))) == 0 ) {
    abort("No information in source {crayon::bold(source)} for countries in 'gdp'.")
  }

  this_e <- environment()
  out <- list("gdp" = gdp,
              "unit_in" = unit_in,
              "unit_out" = unit_out) %>%
    {if (exists("base_x", envir = this_e, inherits = FALSE)) c(., "base_x" = base_x) else .} %>%
    {if (exists("base_y", envir = this_e, inherits = FALSE)) c(., "base_y" = base_y) else .}

  return(out)
}



#' Take gdp data at regional level and disaggregate to country level
#'
#' @param weight_unit A string, either "PPP" or "MER
#' @param weight_year An integer equal to the base_x year
#' @inheritParams convertGDP
#'
#' @return A tibble with the values gdp at country level
disaggregate_regions <- function (gdp, with_regions, weight_unit, weight_year, source) {

  # Get GDP variable from source object, with its unit
  regex_var <- "GDP, PPP \\(constant .... international \\$\\)"
  regex_year <- "GDP, PPP \\(constant (....) international \\$\\)"
  share_var <- grep(regex_var, colnames(eval(rlang::sym(source))), value = TRUE)[1]
  share_year <- stringr::str_match(share_var, regex_year)[,2]
  unit_in <- paste("constant", share_year, "Int$PPP")

  # Convert that variable to desired unit, and compute shares of GDP per region
  if (weight_unit == "PPP") {
    unit_out <- paste("constant", weight_year, "Int$PPP")
  } else {
    unit_out <- paste("constant", weight_year, "US$MER")
  }

  shares <- eval(rlang::sym(source)) %>%
    dplyr::select("iso3c", "year", "value" = tidyselect::all_of(share_var)) %>%
    dplyr::left_join(with_regions, by = "iso3c") %>%
    dplyr::filter(.data$year == weight_year, !is.na(.data$gdpuc_region)) %>%
    convertGDP(unit_in, unit_out, source = source) %>%
    dplyr::group_by(.data$gdpuc_region) %>%
    dplyr::mutate(share = .data$value / sum(.data$value, na.rm = TRUE), .keep = "unused") %>%
    dplyr::ungroup() %>%
    dplyr::select(-.data$year) %>%
    suppressWarnings()

  # Dissagregate regions
  gdp %>%
    dplyr::rename("gdpuc_region" = .data$iso3c) %>%
    dplyr::right_join(with_regions, by = "gdpuc_region") %>%
    dplyr::left_join(shares, by = c("gdpuc_region", "iso3c")) %>%
    dplyr::mutate(value = .data$value * .data$share, .keep = "unused")
}




#' Transform user input for package internal use
#'
#' @param x A tibble, the result of the internal conversion
#' @inheritParams convertGDP
#'
#' @return x, with the same type and names of gdp
transform_internal <- function(x, gdp, with_regions) {

  if (!is.null(with_regions)) {
    x <- x %>%
      dplyr::group_by(.data$gdpuc_region, .data$year) %>%
      dplyr::summarise(value = sum(.data$value, na.rm = TRUE)) %>%
      dplyr::rename("iso3c" = .data$gdpuc_region)
  }

  # Transform into original gdp type
  if (class(gdp)[1] == "magpie"){
    x <- magclass::as.magpie(x[,-1], spatial="iso3c", temporal="year")
    return(x)
  }

  # Get original iso3c and year column names
  if (! "iso3c" %in% colnames(gdp)) {
    i_iso3c <- gdp %>%
      dplyr::select(tidyselect::vars_select_helpers$where(
        ~ (is.character(.x) || is.factor(.x)) && nchar(as.character(.x[[1]])) == 3
      )) %>%
      colnames()
    x <- x %>% dplyr::rename(!!rlang::sym(i_iso3c) := "iso3c")
  }
  if (! "year" %in% colnames(gdp)) {
    i_year <- gdp %>%
      dplyr::select(tidyselect::vars_select_helpers$where(
        ~ is.numeric(.x) & !is.na(.x[[1]]) & nchar(as.character(.x[[1]])) == 4
      )) %>%
      colnames()
    x <- x %>% dplyr::rename(!!rlang::sym(i_year) := "year")
  }

  x
}
