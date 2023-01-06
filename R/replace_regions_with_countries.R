# Replace any regions with the countries they comprise.
replace_regions_with_countries <- function(gdp, unit_in, base_x, with_regions, source) {
  # Separate regions (excluding regions that are countries) from countries
  my_reg <- intersect(unique(gdp$iso3c),
                      setdiff(unique(with_regions$region),
                              unique(with_regions$iso3c)))

  gdp_reg <- dplyr::filter(gdp, .data$iso3c %in% my_reg)
  gdp <- dplyr::filter(gdp, ! .data$iso3c %in% my_reg)

  # Disaggregate regions
  weight_unit <- sub("\\$", "", regmatches(unit_in, regexpr("\\$(...)", unit_in)))
  weight_year <- base_x
  with_regions <- dplyr::rename(with_regions, "gdpuc_region" = "region")
  gdp_reg <- disaggregate_regions(gdp_reg, with_regions, weight_unit, weight_year, source)

  # Bind original countries and countries from disaggregation (duplicates now possible)
  gdp <- dplyr::bind_rows(gdp, gdp_reg)
}



# Take gdp data at regional level and disaggregate to country level according
# to a region mapping.
disaggregate_regions <- function(gdp, with_regions, weight_unit, weight_year, source) {

  # Get GDP variable from source object, with its unit
  regex_var <- "GDP, PPP \\(constant .... international \\$\\)"
  regex_year <- "GDP, PPP \\(constant (....) international \\$\\)"
  share_var <- grep(regex_var, colnames(source), value = TRUE)[1]
  share_year <- regmatches(share_var, regexpr("[[:digit:]]{4}", share_var))
  unit_in <- paste("constant", share_year, "Int$PPP")

  # Convert that variable to desired unit, and compute shares of GDP per region
  if (weight_unit == "PPP") {
    unit_out <- paste("constant", weight_year, "Int$PPP")
  } else {
    unit_out <- paste("constant", weight_year, "US$MER")
  }

  shares <- source %>%
    dplyr::select("iso3c", "year", "value" = tidyselect::all_of(share_var)) %>%
    dplyr::left_join(with_regions, by = "iso3c") %>%
    dplyr::filter(.data$year == weight_year, !is.na(.data$gdpuc_region)) %>%
    convertGDP(unit_in, unit_out, source = source, verbose = FALSE) %>%
    dplyr::group_by(.data$gdpuc_region) %>%
    dplyr::mutate(share = .data$value / sum(.data$value, na.rm = TRUE), .keep = "unused") %>%
    dplyr::ungroup() %>%
    dplyr::select(-"year") %>%
    suppressWarnings()

  # Dissagregate regions
  gdp %>%
    dplyr::rename("gdpuc_region" = "iso3c") %>%
    dplyr::left_join(with_regions, by = "gdpuc_region") %>%
    dplyr::left_join(shares, by = c("gdpuc_region", "iso3c")) %>%
    dplyr::mutate(value = .data$value * .data$share, .keep = "unused")
}
