# Replace any regions with the countries they comprise.
replace_regions_with_countries <- function(gdp, unit_in, base_x, with_regions, source) {
  # Separate regions (excluding regions that are countries) from countries
  my_reg <- intersect(unique(gdp$iso3c),
                      setdiff(unique(with_regions$region),
                              unique(with_regions$iso3c)))

  gdp_reg <- dplyr::filter(gdp, .data$iso3c %in% my_reg)
  gdp <- dplyr::filter(gdp, ! .data$iso3c %in% my_reg)

  # Disaggregate regions
  with_regions <- dplyr::rename(with_regions, "gdpuc_region" = "region")
  gdp_reg <- disaggregate_regions(gdp_reg, with_regions, unit_in, base_x, source)

  # Bind original countries and countries from disaggregation (duplicates now possible)
  gdp <- dplyr::bind_rows(gdp, gdp_reg)
}



# Take gdp data at regional level and disaggregate to country level according to a region mapping.
disaggregate_regions <- function(gdp, with_regions, unit_in, base_x, source) {
  weight_year <- base_x

  # Convert the GDP data from source, to that of unit_in (here unit_in is already transformed)
  if (grepl("IntPPP", unit_in)) {
    regex_var <- "GDP, PPP \\(constant .... international \\$\\)"
    regex_year <- "GDP, PPP \\(constant (....) international \\$\\)"
    share_var <- grep(regex_var, colnames(source), value = TRUE)[1]
    share_year <- regmatches(share_var, regexpr("[[:digit:]]{4}", share_var))
    unit_in <- paste("constant", share_year, "Int$PPP")
    unit_out <- paste("constant", weight_year, "Int$PPP")
  } else {
    regex_var <- "GDP \\(constant .... US\\$\\)"
    regex_year <- "GDP \\(constant .... US\\$\\)"
    share_var <- grep(regex_var, colnames(source), value = TRUE)[1]
    share_year <- regmatches(share_var, regexpr("[[:digit:]]{4}", share_var))
    unit_in <- paste("constant", share_year, "US$MER")
    unit_out <- paste("constant", weight_year, "US$MER")
  }

  # Convert the GDP data from source, to that of the users unit_in (here unit_out) and get regional shares
  shares <- source %>%
    dplyr::filter(.data$year == weight_year) %>%
    # Keep only countries with all conversion factors available
    tidyr::drop_na(c(
      "GDP deflator",
      "MER (LCU per US$)",
      "PPP conversion factor, GDP (LCU per international $)")) %>%
    dplyr::select("iso3c", "year", "value" = tidyselect::all_of(share_var)) %>%
    dplyr::left_join(with_regions, by = "iso3c") %>%
    tidyr::drop_na("gdpuc_region") %>%
    convertGDP(unit_in, unit_out, source = source, verbose = FALSE) %>%
    dplyr::mutate(share = .data$value / sum(.data$value), .keep = "unused", .by = "gdpuc_region") %>%
    dplyr::select(-"year") %>%
    suppressWarnings()

  cli_inform(function() cli::cli_alert_info("Dissaggreagting regions using GDP in {unit_out} as weights."))

  # Dissagregate regions
  gdp %>%
    dplyr::rename("gdpuc_region" = "iso3c") %>%
    dplyr::left_join(shares, by = c("gdpuc_region"), relationship = "many-to-many") %>%
    dplyr::mutate(value = .data$value * .data$share, .keep = "unused")
}
