# Transform user input for package internal use
transform_user_input <- function(gdp,
                                 unit_in,
                                 unit_out,
                                 source,
                                 use_USA_cf_for_all,
                                 with_regions,
                                 replace_NAs,
                                 iso3c_column,
                                 year_column) {
  . <- NULL

  # Convert to tibble, if necessary
  if (class(gdp)[1] == "magpie") {
    # Check if the magpie object has 2 spatial dimensions
    spat2 <- magclass::ndim(gdp, dim = 1) == 2
    # Check if the magpie object has year info
    hasYears <- !is.null(magclass::getYears(gdp))
    # Transform to tibble and rename columns
    gdp <- gdp %>% tibble::as_tibble() %>% dplyr::rename("iso3c" = 1)
    if (spat2) gdp <- dplyr::rename(gdp, "spatial2" = 2)
    if (hasYears && !spat2) gdp <- dplyr::rename(gdp, "year" = 2)
    if (hasYears && spat2) gdp <- dplyr::rename(gdp, "year" = 3)
  }

  # Extract base years if they exist, and adjust strings of units
  handle_unit <- function(x, inOrOut) {
    if (grepl("constant", x)) {
      base_year <- as.double(regmatches(x, regexpr("[[:digit:]]{4}", x)))
      x <- sub(paste0(" ", base_year), "", x)
      x <- if (inOrOut == "in") paste0(x, "_base_x") else paste0(x, "_base_y")
    } else {
      base_year <- NULL
    }

    x <- sub("US\\$MER", "USA_CU", x)
    x <- sub("\u20ac|EUR", "DEU_CU", x)
    x <- sub("Int\\$PPP", "IntPPP", x)

    iso3cOfUnit <- sub("_CU", "", regmatches(x, regexpr("..._CU", x)))
    if (!rlang::is_empty(iso3cOfUnit)) x <- sub(paste0(iso3cOfUnit, "_"), "x", x)
    x <- sub(" ", "_", x)

    list("x" = x, "base_year" = base_year, "iso3cOfUnit" = iso3cOfUnit)
  }
  helper_unit_in <- handle_unit(unit_in, "in")
  base_x <- helper_unit_in$base_year
  iso3c_x <- helper_unit_in$iso3cOfUnit
  unit_in <- helper_unit_in$x
  helper_unit_out <- handle_unit(unit_out, "out")
  base_y <- helper_unit_out$base_year
  iso3c_y <- helper_unit_out$iso3cOfUnit
  unit_out <- helper_unit_out$x

  require_year_column <- any(grepl("current", c(unit_in, unit_out)))

  # Rename columns if necessary
  if (! "iso3c" %in% colnames(gdp)) {
    i_iso3c <- smart_select_iso3c(gdp, iso3c_column)
    if (length(i_iso3c) != 1) {
      abort(glue::glue("Invalid 'gdp' argument. `gdp` has no '{iso3c_column}' column, and no other \\
                        column could be identified in its stead."))
    }
    if (i_iso3c != iso3c_column) {
      warn(glue::glue("No '{iso3c_column}' column in 'gdp' argument. Using '{i_iso3c}' column instead."))
    }
    gdp <- dplyr::rename(gdp, "iso3c" = !!rlang::sym(i_iso3c))

  }
  if (require_year_column && ! "year" %in% colnames(gdp)) {
    i_year <- smart_select_year(gdp, year_column)
    if (length(i_year) != 1) {
      abort(glue::glue("Invalid 'gdp' argument. 'gdp' does not have a '{year_column}' column, required when \\
                          converting current values, and no other column could be identified in its stead."))
    }
    if (i_year != year_column) {
      warn(glue::glue("No '{year_column}' column in 'gdp' argument. Using '{i_year}' column instead."))
    }
    gdp <- dplyr::rename(gdp, "year" = !!rlang::sym(i_year))
  }

  # Evaluate source (same steps as performed in check_source)
  source_name <- if (is.character(source)) source else "user_provided"
  source <- check_source(source)

  # If a region mapping is available and a region code (that isn't a country-region) is detected, replace the region
  # with the countries it comprises.
  if (is.character(with_regions)) {
    with_regions <- madrat::toolGetMapping(with_regions) %>%
      dplyr::select("iso3c" = "CountryCode", "region" = "RegionCode")
  }
  if (!is.null(with_regions) &&
      any(gdp$iso3c %in% with_regions$region & !gdp$iso3c %in% with_regions$iso3c)) {
    gdp <- replace_regions_with_countries(gdp, unit_in, base_x, with_regions, source)
  }

  # Check that base_y and base_x years exist in the source
  if (!is.null(base_y) && !base_y %in% source$year) {
    abort("Incompatible 'unit_out' and 'source'. No information in source {crayon::bold(source_name)} for the \\
          year {base_y}.")
  }
  if (!is.null(base_x) && !base_x %in% source$year) {
    abort("Incompatible 'unit_in' and 'source'. No information in source {crayon::bold(source_name)} for the \\
          year {base_x}.")
  }
  # Check general overlap
  if (require_year_column && length(intersect(unique(gdp$year), unique(source$year))) == 0) {
    abort("Incompatible 'gdp' and 'source'. No information in source {crayon::bold(source_name)} for years in 'gdp'.")
  }

  # Use different source if required by the use_USA_cf_for_all and replace_NAs argument
  if (use_USA_cf_for_all) source <- adapt_source_USA(gdp, source)
  if (!use_USA_cf_for_all &&
      (!is.null(replace_NAs) && !any(sapply(c(NA, 0, "no_conversion"), setequal, replace_NAs))) ) {
    source <- adapt_source(gdp, source, with_regions, replace_NAs, require_year_column)
    source_name <- paste0(source_name, "_adapted")
  }

  if (length(intersect(unique(gdp$iso3c), unique(source$iso3c))) == 0) {
    abort("No information in source {crayon::bold(source_name)} for countries in 'gdp'.")
  }

  list("gdp" = gdp,
       "unit_in" = unit_in,
       "unit_out" = unit_out,
       "iso3c_x" = iso3c_x,
       "iso3c_y" = iso3c_y,
       "base_x" = base_x,
       "base_y" = base_y,
       "require_year_column" = require_year_column,
       "source" = source,
       "source_name" = source_name)
}





# Transform user input for package internal use
transform_internal <- function(x, gdp, with_regions, require_year_column, iso3c_column, year_column) {

  if (!is.null(with_regions) && "gdpuc_region" %in% colnames(x)) {
    x_reg <- dplyr::filter(x, !is.na(.data$gdpuc_region))
    x <- x %>%
      dplyr::filter(is.na(.data$gdpuc_region)) %>%
      dplyr::select(-"gdpuc_region")

    x_reg <- x_reg %>%
      dplyr::group_by(dplyr::across(c(-"iso3c", -"value"))) %>%
      dplyr::summarise(value = sum(.data$value, na.rm = TRUE), .groups = "drop") %>%
      dplyr::rename("iso3c" = "gdpuc_region")

    i_iso3c <- if (! "iso3c" %in% colnames(gdp)) smart_select_iso3c(gdp) else "iso3c"

    x <- x %>%
      dplyr::bind_rows(x_reg) %>%
      dplyr::arrange(factor(.data$iso3c, levels = unique(gdp[[i_iso3c]])))
  }

  # Transform into original gdp type
  if (class(gdp)[1] == "magpie") {
    # Check if the original magpie object had 2 spatial dimensions
    spat2 <- all(grepl("\\.", magclass::getItems(gdp, dim = 1)))
    if (!spat2) {
      x <- magclass::as.magpie(x, spatial = "iso3c", temporal = "year", datacol = "value")
    } else {
      x <- magclass::as.magpie(x, spatial = c("iso3c", "spatial2"), temporal = "year", datacol = "value")
    }
    magclass::getSets(x) <- magclass::getSets(gdp)
    return(x)
  }

  # Get original iso3c and year column names
  if (! "iso3c" %in% colnames(gdp)) {
    i_iso3c <- smart_select_iso3c(gdp, iso3c_column)
    x <- dplyr::rename(x, !!rlang::sym(i_iso3c) := "iso3c")
  }
  if (require_year_column && ! "year" %in% colnames(gdp)) {
    i_year <- smart_select_year(gdp, year_column)
    x <- dplyr::rename(x, !!rlang::sym(i_year) := "year")
  }

  x
}
