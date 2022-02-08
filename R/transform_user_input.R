# Transform user input for package internal use
transform_user_input <- function(gdp, unit_in, unit_out, source, with_regions, replace_NAs) {
  . <- NULL

  # Convert to tibble, if necessary
  if (class(gdp)[1] == "magpie") {
    gdp <- mag_2_tibb(gdp)
  }

  # Extract base years if they exist, and adjust string
  if (grepl("constant", unit_in)) {
    base_x <- regmatches(unit_in, regexpr("[[:digit:]]{4}", unit_in)) %>% as.double()
    unit_in <- sub(base_x, "YYYY", unit_in) %>%
      paste0(" base x")
  }
  if (grepl("constant", unit_out)) {
    base_y <- regmatches(unit_out, regexpr("[[:digit:]]{4}", unit_out)) %>% as.double()
    unit_out <- sub(base_y, "YYYY", unit_out) %>%
      paste0(" base y")
  }

  # Rename columns if necessary
  if (! "iso3c" %in% colnames(gdp)) {
    i_iso3c <- smart_select_iso3c(gdp)
    if (length(i_iso3c) != 1) {
      abort("Invalid 'gdp' argument. `gdp` has no 'iso3c' column, and no other \\
               column could be identified in its stead.")
    }
    warn("No 'iso3c' column in 'gdp' argument. Using '{i_iso3c}' column instead.")
    gdp <- dplyr::rename(gdp, "iso3c" = !!rlang::sym(i_iso3c))
  }
  if (! "year" %in% colnames(gdp)) {
    i_year <- smart_select_year(gdp)
    if (length(i_year) != 1) {
      abort("Invalid 'gdp' argument. 'gdp' does not have the required \\
               'year' column, and no other column could be identified in its stead.")
    }
    warn("No 'year' column in 'gdp' argument. Using '{i_year}' column instead.")
    gdp <- dplyr::rename(gdp, "year" = !!rlang::sym(i_year))
  }

  # Evaluate source (same steps as performed in check_source)
  source_name <- if (is.character(source)) source else "user_provided"
  source <- check_source(source)

  # If a region mapping is available and a region code (that isn't a
  # country-region) is detected, replace the region with the countries it
  # comprises.
  if (!is.null(with_regions) &&
      any(gdp$iso3c %in% with_regions$region & !gdp$iso3c %in% with_regions$iso3c)) {
    gdp <- replace_regions_with_countries(gdp, unit_in, base_x, with_regions, source)
  }

  # Need this to check for existence of base_y and base_x
  this_e <- environment()

  # Check that base_y and base_x years exist in the source
  if (exists("base_y", envir = this_e, inherits = FALSE) && !base_y %in% source$year) {
    abort("No information in source {crayon::bold(source_name)} for year {base_y} in 'unit_out'.")
  }
  if (exists("base_x", envir = this_e, inherits = FALSE) && !base_x %in% source$year) {
    abort("No information in source {crayon::bold(source_name)} for year {base_x} in 'unit_in'.")
  }
  # Check general overlap
  if (length(intersect(unique(gdp$year), unique(source$year))) == 0) {
    abort("No information in source {crayon::bold(source_name)} for years in 'gdp'.")
  }

  # Use different source if required
  if (!is.null(replace_NAs) && replace_NAs != 0) {
    source <- adapt_source(gdp, source, with_regions, replace_NAs)
    source_name <- paste0(source_name, "_adapted")
  }

  if (length(intersect(unique(gdp$iso3c), unique(source$iso3c))) == 0) {
    abort("No information in source {crayon::bold(source_name)} for countries in 'gdp'.")
  }

  list("gdp" = gdp,
       "unit_in" = unit_in,
       "unit_out" = unit_out,
       "source" = source,
       "source_name" = source_name) %>%
    {if (exists("base_x", envir = this_e, inherits = FALSE)) c(., "base_x" = base_x) else .} %>%
    {if (exists("base_y", envir = this_e, inherits = FALSE)) c(., "base_y" = base_y) else .}
}





# Transform user input for package internal use
transform_internal <- function(x, gdp, with_regions) {

  if (!is.null(with_regions) && "gdpuc_region" %in% colnames(x)) {
    x_reg <- dplyr::filter(x, !is.na(.data$gdpuc_region))
    x <- x %>%
      dplyr::filter(is.na(.data$gdpuc_region)) %>%
      dplyr::select(-.data$gdpuc_region)

    x_reg <- x_reg %>%
      dplyr::group_by(dplyr::across(c(-.data$iso3c, -.data$value))) %>%
      dplyr::summarise(value = sum(.data$value, na.rm = TRUE), .groups = "drop") %>%
      dplyr::rename("iso3c" = .data$gdpuc_region)

    i_iso3c <- if (! "iso3c" %in% colnames(gdp)) smart_select_iso3c(gdp) else "iso3c"

    x <- x %>%
      dplyr::bind_rows(x_reg) %>%
      dplyr::arrange(factor(.data$iso3c, levels = unique(gdp[[i_iso3c]])))
  }

  # Transform into original gdp type
  if (class(gdp)[1] == "magpie") {
    x <- magclass::as.magpie(x, spatial = "iso3c", temporal = "year")
    magclass::getSets(x) <- magclass::getSets(gdp)
    return(x)
  }

  # Get original iso3c and year column names
  if (! "iso3c" %in% colnames(gdp)) {
    i_iso3c <- smart_select_iso3c(gdp)
    x <- dplyr::rename(x, !!rlang::sym(i_iso3c) := "iso3c")
  }
  if (! "year" %in% colnames(gdp)) {
    i_year <- smart_select_year(gdp)
    x <- dplyr::rename(x, !!rlang::sym(i_year) := "year")
  }

  x
}
